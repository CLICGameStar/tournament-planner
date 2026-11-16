package planner

import scala.math.Ordered.orderingToOrdered

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime}
import scala.io.Source
import java.time.Duration
import scala.util.Random

@main def findSchedule(): Unit =
  import Planner.*

  val (gameNames, conflicts) = parseConflictMatrix("src/main/resources/conflict_matrix.csv")
  val updatedConflicts = adjustConflicts(conflicts, independentSets)
  val constraints = Constraints(
    totalDuration = endHour,
    breaks = breaks,
    independentSets = independentSets
  )

  val schedules =
    (0 until 1000).toList
      .map(x =>
        Random.setSeed(x)
        (x, iterativeSchedule(games, updatedConflicts, constraints, 100).sorted)
      ).filter((x, sched) =>
        sched.size == games.size
      )
      .distinctBy(_._2)

  val smallestConflict = schedules.map((x, sched) => calculateTotalConflicts(sched, updatedConflicts)).min

  schedules
    .filter((x, sched) => calculateTotalConflicts(sched, updatedConflicts) == smallestConflict)
    .foreach((x, sched) =>
      println(f"Optimized schedule conflicts $x: " + calculateTotalConflicts(sched, updatedConflicts))
      printSchedule(sched, startTime)
    )
  println("Found " + schedules.size + " distinct schedules")

  // To test a single schedule :
  // val schedule = iterativeSchedule(games, updatedConflicts, constraints, 100).sorted
  // assert(schedule.size == games.size)
  // println("Optimized schedule conflicts: " + calculateTotalConflicts(schedule, updatedConflicts))
  // printSchedule(schedule, startTime)

object Planner:
  import Time.*

  type ConflictMatrix = Map[(String, String), Int]

  case class Constraints(
      totalDuration: Int,
      breaks: List[Slot],
      independentSets: Set[Set[String]]
  )

  case class Game(name: String, duration: Int, forbiddenSlots: List[Slot] = List())

  case class Slot(start: Int, end: Int):
    def this(start: Time, end: Time) =
      this(startTime.timeTo(start), startTime.timeTo(end))
    def overlaps(that: Slot): Boolean =
      Math.max(this.start, that.start) < Math.min(this.end, that.end)
  object Slot:
    def apply(start: Time, end: Time): Slot = new Slot(start, end)

  case class ScheduleSlot(start: Int, game: Game) extends Ordered[ScheduleSlot]:

    override def compare(that: ScheduleSlot): Int =
      if this.start != that.start then
        this.start.compareTo(that.start)
      else if this.end != that.end then
        this.end.compareTo(that.end)
      else
        this.game.name.compareTo(that.game.name)

    def duration = game.duration
    def end = start + game.duration
    def toSlot = Slot(start, start + game.duration)
    def shift(shift: Int) = ScheduleSlot(start + shift, game)

    def overlaps(that: ScheduleSlot): Boolean =
      this.toSlot.overlaps(that.toSlot)
    def overlaps(that: Slot): Boolean =
      this.toSlot.overlaps(that)

  def parseConflictMatrix(path: String): (List[String], ConflictMatrix) =
    val lines = Source.fromFile(path).getLines().toList
    val headers = lines.head.split(",").tail.map(_.trim).toList
    val matrix = lines.tail.flatMap { line =>
      val cols = line.split(",").map(_.trim)
      val rowGame = cols.head
      cols.tail.zipWithIndex.map {
        case (value, colIndex) =>
          val colGame = headers(colIndex)
          ((rowGame, colGame), value.toInt)
      }
    }.toMap
    (headers, matrix)

  def adjustConflicts(conflicts: ConflictMatrix, independentSets: Set[Set[String]]): ConflictMatrix =
    conflicts.map {
      case (games @ (g1, g2), conflict) if g1 == g2 => (games, 0)
      case other                                    => other
    }

  def maxTotalConflicts(conflicts: ConflictMatrix): Int =
    calculateTotalConflicts(games.map(ScheduleSlot(0, _)), conflicts)

  def calculateConflict(game1: Game, game2: Game, conflicts: ConflictMatrix): Int =
    conflicts.getOrElse((game1.name, game2.name), 0)

  def calculateGameConflict(gameSlot: ScheduleSlot, schedule: List[ScheduleSlot], conflicts: ConflictMatrix): Int =
    schedule.filter(gameSlot.overlaps(_)).map(game => calculateConflict(game.game, gameSlot.game, conflicts)).sum

  def calculateTotalConflicts(schedule: List[ScheduleSlot], conflicts: ConflictMatrix): Int =
    schedule.combinations(2).collect {
      case List(slot1, slot2) if slot1.overlaps(slot2) =>
        calculateConflict(slot1.game, slot2.game, conflicts)
    }.sum

  def isValidSlot(gameSlot: ScheduleSlot, constraints: Constraints) =
    gameSlot.end <= constraints.totalDuration
      && constraints.breaks.forall(!gameSlot.overlaps(_))
      && gameSlot.game.forbiddenSlots.forall(!gameSlot.overlaps(_))

  def closestValidSlot(gameSlot: ScheduleSlot, constraints: Constraints): Option[ScheduleSlot] =
    if isValidSlot(gameSlot, constraints) then
      Some(gameSlot)
    else
      if gameSlot.start + gameSlot.duration > constraints.totalDuration then None
      else closestValidSlot(gameSlot.shift(1), constraints)

  def createBasicSchedule(games: List[Game], constraints: Constraints): List[ScheduleSlot] =
    games.flatMap(game => closestValidSlot(ScheduleSlot(0, game), constraints))

  def allValidGameSlots(game: Game, constraints: Constraints): List[ScheduleSlot] =
    (0 until constraints.totalDuration).toList
      .flatMap(start => closestValidSlot(ScheduleSlot(start, game), constraints))
      .distinct

  def respectsIndependentSets(gameSlot: ScheduleSlot, schedule: List[ScheduleSlot], constraints: Constraints): Boolean =
    schedule.forall(slot =>
      constraints.independentSets.forall(set =>
        slot == gameSlot || !gameSlot.overlaps(slot) || !set.contains(slot.game.name) || !set.contains(
          gameSlot.game.name
        )
      )
    )

  def findBestSlot(
      game: Game,
      schedule: List[ScheduleSlot],
      conflicts: ConflictMatrix,
      constraints: Constraints
  ): Option[ScheduleSlot] =
    Random.shuffle(allValidGameSlots(game, constraints))
      .filter(slot => respectsIndependentSets(slot, schedule, constraints))
      .minByOption(slot => calculateGameConflict(ScheduleSlot(slot.start, game), schedule, conflicts))

  def iterativeSchedule(
      games: List[Game],
      conflicts: ConflictMatrix,
      constraints: Constraints,
      iterations: Int = 10
  ): List[ScheduleSlot] =

    def optimize(schedule: List[ScheduleSlot], iterations: Int): List[ScheduleSlot] =
      if iterations <= 0 then schedule
      else
        val newSchedule =
          schedule.foldLeft(schedule)((sched, slot) =>
            val schedWithoutSlot = sched.filterNot(_ == slot)
            findBestSlot(slot.game, schedWithoutSlot, conflicts, constraints) match
              case Some(newSlot) => newSlot :: schedWithoutSlot
              case _             => sched
          )

        optimize(
          if calculateTotalConflicts(newSchedule, conflicts) < calculateTotalConflicts(schedule, conflicts) then
            newSchedule
          else Random.shuffle(schedule),
          iterations - 1
        )

    optimize(randomSchedule(games, constraints), iterations)

  def onePassSchedule(
      games: List[Game],
      conflicts: ConflictMatrix,
      constraints: Constraints
  ) =
    games.foldLeft((List[ScheduleSlot]()))((schedule, game) =>
      findBestSlot(game, schedule, conflicts, constraints) match
        case Some(slot) =>
          slot :: schedule
        case None =>
          schedule
    )

  def randomSchedule(games: List[Game], constraints: Constraints) =
    Random.shuffle(games).foldLeft(List.empty[ScheduleSlot]) { (schedule, game) =>
      val validSlots = Random.shuffle(allValidGameSlots(game, constraints)).filter(slot =>
        respectsIndependentSets(slot, schedule, constraints)
      )
      validSlots.headOption match
        case Some(slot) => slot :: schedule
        case None       => schedule
    }

  def printSchedule(schedule: List[ScheduleSlot], startHour: Time): Unit =
    schedule.foreach(x =>
      println(
        s"${startHour.plusHours(x.start)}h - " +
          s"${startHour.plusHours(x.start + x.game.duration)}h : " +
          s"${gameFancyNames.getOrElse(x.game.name, x.game.name)}"
      )
    )

  enum Time(hour: Int, day: Int, month: Int, year: Int):
    case Sat(hour: Int) extends Time(hour, 5, 12, 2026)
    case Sun(hour: Int) extends Time(hour, 6, 12, 2026)

    val formatter = DateTimeFormatter.ofPattern("EEE HH")
    val dateTime: LocalDateTime = LocalDateTime.of(year, month, day, hour, 0)

    override def toString(): String = dateTime.format(formatter)
    def timeTo(that: Time): Int =
      Duration.between(this.dateTime, that.dateTime).toHours.toInt
    def plusHours(hours: Int): Time =
      this match
        case Sat(hour) if hour + hours < 24 => Sat(hour + hours)
        case Sat(hour)                      => Sun((hour + hours) % 24)
        case Sun(hour) if hour + hours < 24 => Sun(hour + hours)
        case _                              => throw IllegalArgumentException("Cannot add hours beyond Sunday")

  val startTime = Sat(10)
  val endTime = Sun(7)
  val startHour = 0
  val endHour = startTime.timeTo(endTime)

  val breaks = List(Slot(Sat(12), Sat(13)), Slot(Sat(19), Sat(20)))

  val noNight = List(Slot(Sat(23), Sun(8)))
  val noDeepNight = List(Slot(Sun(2), endTime))
  val noLateAf = List(Slot(Sun(3), endTime))
  val noMorning = List(Slot(startTime, Sat(12)))
  val noEarly = List(Slot(startTime, Sat(18)))
  val onlyAfternoon = List(
    Slot(startTime, Sat(12)),
    Slot(Sat(18), endTime)
  )
  val closedSwitchRoom = List(Slot(Sun(1), endTime))

  val games = List(
    Game("LoL", 5),
    Game("Valo", 3),
    Game("RL_2v2", 3),
    Game("CS2", 4),
    Game("Overwatch2", 3),
    Game("Minecraft", 2, noNight),
    Game("Brawlhalla_2v2", 2),
    Game("GGST", 2),
    Game("MicroWorks", 1, noNight),
    Game("Overcooked2", 1, noNight),
    Game("UCH", 2, closedSwitchRoom),
    Game("Geoguessr", 1),
    Game("BAPBAP", 2),
    Game("Pokemon_Showdown", 2),
    Game("MK8dx", 4, closedSwitchRoom),
    Game("Smash_1v1", 4, closedSwitchRoom),
    Game("Codenames", 3, noNight),
    Game("Ping-pong", 4),
    Game("Babyfoot", 3),
    Game("Trivia", 2, noNight)
  )

  val independentSets = Set(
    // Jeux switch
    Set("MK8dx", "Smash_1v1"),
    // Jeux hors ligne
    Set("Ping-pong", "Babyfoot", "Codenames", "Trivia"),
    // Jeux de zinzins
    Set("LoL", "Valo", "CS2"),
    // Fighting games
    Set("GGST", "Smash_1v1", "Brawlhalla_2v2")
  )

  val gameFancyNames = Map(
    "LoL" -> "League of Legends",
    "Valo" -> "Valorant",
    "RL_2v2" -> "Rocket League (2v2)",
    "CS2" -> "Counter Strike 2",
    "Overwatch2" -> "Overwatch",
    "Minecraft" -> "Minecraft",
    "Brawlhalla_2v2" -> "Brawlhalla (2v2)",
    "GGST" -> "Guilty Gear Strive",
    "MicroWorks" -> "MicroWorks",
    "Overcooked2" -> "Overcooked 2 (Duo)",
    "UCH" -> "Ultimate Chicken Horse",
    "Geoguessr" -> "GeoGuessr",
    "BAPBAP" -> "BAPBAP",
    "Pokemon_Showdown" -> "Pokémon Showdown",
    "MK8dx" -> "Mario Kart 8 Deluxe",
    "Smash_1v1" -> "Super Smash Bros Ultimate (1v1)",
    "Codenames" -> "Codenames",
    "Ping-pong" -> "Ping Pong",
    "Babyfoot" -> "Babyfoot",
    "Trivia" -> "Trivia"
  )
