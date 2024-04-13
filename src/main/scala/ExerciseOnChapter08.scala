import cats.effect.IO
import ch08_SchedulingMeetings.calendarEntriesApiCall
import ch08_SchedulingMeetings.createMeetingApiCall
import ch08_CardGame.castTheDie
import ch08_CardGame.drawAPointCard
import cats.implicits._ // import が必要

object ExerciseOnChapter08 {

  def calendarEntries(name: String): IO[List[MeetingTime]] =
    IO.delay(calendarEntriesApiCall(name))

  def createMeeting(names: List[String], meetingTime: MeetingTime): IO[Unit] =
    IO.delay(createMeetingApiCall(names, meetingTime))

  def scheduledMeetings(attendees: List[String]): IO[List[MeetingTime]] =
    attendees
      .map(attendee => retry(calendarEntries(attendee), 10))
      .sequence // converts a List of IO into IO of List
      .map(_.flatten)

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
    attendees: List[String],
    lengthHours: Int,
    saveMeeting: (List[String], MeetingTime) => IO[Unit],
  ): IO[Option[MeetingTime]] =
    for {
      existingMeetings <- scheduledMeetings(attendees)
      possibleMeeting = possibleMeetings(
        existingMeetings,
        startHour = 8,
        endHour = 16,
        lengthHours = lengthHours,
      ).headOption

      _ <- possibleMeeting match
        case Some(meeting) => saveMeeting(attendees, meeting)
        case None => IO.unit
    } yield possibleMeeting

  def schedulingProgram(
    getName: IO[String],
    showMeeting: Option[MeetingTime] => IO[Unit]
  ): IO[Unit] =
    for {
      name1 <- getName
      name2 <- getName
      possibleMeeting <- schedule(List(name1, name2), 2, createMeeting)
      _ <- showMeeting(possibleMeeting)
    } yield ()

  def retry[A](action: IO[A], maxRetries: Int): IO[A] =
    List.range(0, maxRetries)
      .map(_ => action)
      .foldLeft(action) { (program, retryAction) =>
        program.orElse(retryAction)
      }
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
