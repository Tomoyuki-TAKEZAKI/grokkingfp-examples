import cats.effect.IO
import ch08_SchedulingMeetings.calendarEntriesApiCall
import ch08_SchedulingMeetings.createMeetingApiCall

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
