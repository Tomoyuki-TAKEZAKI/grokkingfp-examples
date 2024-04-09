import cats.effect.IO
import ch08_SchedulingMeetings.calendarEntriesApiCall
import ch08_SchedulingMeetings.createMeetingApiCall
import ch08_CardGame.castTheDie
import ch08_CardGame.drawAPointCard

object ExerciseOnChapter08 {

  def calendarEntries(name: String): IO[List[MeetingTime]] =
    IO.delay(calendarEntriesApiCall(name))

  def createMeeting(names: List[String], meetingTime: MeetingTime): IO[Unit] =
    IO.delay(createMeetingApiCall(names, meetingTime))

  def scheduledMeetings(person1: String, person2: String): IO[List[MeetingTime]] =
    for {
      schedule1 <- calendarEntries(person1)
      schedule2 <- calendarEntries(person2)
    } yield schedule1.appendedAll(schedule2)

  def possibleMeetings(
    existingMeetings: List[MeetingTime],
    startHour: Int,
    endHour: Int,
    lengthHours: Int,
  ): List[MeetingTime] =
    List.range(startHour, endHour - lengthHours + 1) // possible start hours
      .map(startHour =>
        MeetingTime(startHour, startHour + lengthHours))
      .filter(possibleMeeting =>
        existingMeetings.forall(meetingNotOverlaps(_, possibleMeeting))
      )

  private def meetingNotOverlaps(
    meeting1: MeetingTime,
    meeting2: MeetingTime,
  ): Boolean =
    meeting1.endHour <= meeting2.startHour || meeting2.endHour <= meeting1.startHour

  def schedule(
    person1: String,
    person2: String,
    lengthHours: Int,
  ): IO[Option[MeetingTime]] =
    scheduledMeetings(person1, person2)
      .map(meetings =>
        possibleMeetings(
          meetings,
          startHour = 8,
          endHour = 16,
          lengthHours = lengthHours,
        )
          .headOption
      )
}

object Exercise0827 {

  // 1
  def exercise01(): IO[Int] =
    IO.delay(castTheDie())
      .orElse(IO.pure(0))

  // 2
  def exercise02(): IO[Int] =
    IO.delay(drawAPointCard())
      .orElse(IO.delay(castTheDie()))

  // 3
  def exercise03(): IO[Int] =
    IO.delay(castTheDie())
      .orElse(IO.delay(castTheDie()))
      .orElse(IO.pure(0))

  // 4
  def exercise04(): IO[Int] =
    for {
      die <- IO.delay(castTheDie()).orElse(IO.pure(0))
      card <- IO.delay(castTheDie()).orElse(IO.pure(0))
    } yield die + card

  // 5
  def exercise05(): IO[Int] =
    val trial = for {
      card <- IO.delay(drawAPointCard())
      die1 <- IO.delay(castTheDie())
      die2 <- IO.delay(castTheDie())
    } yield card + die1 + die2

    trial.orElse(IO.pure(0))
}
