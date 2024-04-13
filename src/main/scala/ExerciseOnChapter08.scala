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
    attendees.flatTraverse(attendee => retry(calendarEntries(attendee), 10))

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

object Exercise0838 {
  def f01[A, B](x: IO[A], f: A => B): IO[B] = ???

  def f02[A](x: IO[IO[A]]): IO[A] = ???

  def f03[A, B](x: IO[A], f: A => IO[B]): IO[B] = ???

  def f04[A](x: A): IO[A] = ???

  def f05[A](impureAction: () => A): IO[A] = ???

  def f06[A](x: IO[A], alternative: IO[A]): IO[A] = ???

  def f07[A](x: List[IO[A]]): IO[List[A]] = ???

  def f08[A](x: Option[IO[A]]): IO[Option[A]] = ???

  def f09[A, B](x: List[A], y: List[A]): List[A] = ???

  def f10[A](x: List[A], f: A => Boolean): List[A] = ???

  def f11[A](x: List[A], zero: A, f: (A, A) => A): A = ???

  def f12[A](x: List[List[A]]): List[A] = ???

  def f13[A, B](x: List[A], f: A => List[B]): List[B] = ???

  def f14[A](x: List[A], f: A => Boolean): Boolean = ???

  def f15[A, B](x: Set[A], f: A => B): Set[B] = ???

  def f16[A](x: Set[A], f: A => Boolean): Set[A] = ???

  def f17[A](x: Set[A], zero: A, f: (A, A) => A): A = ???

  def f18[A](x: Set[Set[A]]): Set[A] = ???

  def f19[A, B](x: Set[A], f: A => Set[B]): Set[B] = ???

  def f20[A](x: Set[A], f: A => Boolean): Boolean = ???

  def f21[A, B](x: Option[A], f: A => B): Option[B] = ???

  def f22[A](x: Option[A], f: A => Boolean): Option[A] = ???

  def f23[A](x: Option[A], zero: A, f: (A, A) => A): A = ???

  def f24[A](x: Option[Option[A]]): Option[A] = ???

  def f25[A, B](x: Option[A], f: A => Option[B]): Option[B] = ???

  def f26[A](x: Option[A], f: A => Boolean): Boolean = ???

  def f27(x: String): Option[Int] = ???

  def f28[A](x: Option[A], alternative: Option[A]): Option[A] = ???

  def f29[A, B](x: Option[A], y: B): Either[B, A] = ???

  def f30[A, B](x: Option[A], y: B): Either[A, B] = ???

  def f31[A](x: List[Option[A]]): Option[List[A]] = ???

  def f32[A, B, C](x: Either[A, B], f: B => C): Either[A, C] = ???

  def f33[A, B, C](x: Either[A, B], zero: C, f: (C, B) => C): C = ???

  def f34[A, B](x: Either[A, Either[A, B]]): Either[A, B] = ???

  def f35[A, B, C](x: Either[A, B], f: B => Either[A, C]): Either[A, C] = ???

  def f36[A, B](x: Either[A, B], f: B => Boolean): Boolean = ???

  def f37[A, B](x: Either[A, B], alternative: Either[A, B]): Either[A, B] = ???

  def f38[A, B](x: Either[A, B]): Option[B] = ???

  def f39[A, B](x: List[Either[A, B]]): Either[A, List[B]] = ???

  def f40[A, B](x: Either[A, List[B]]): List[Either[A, B]] = ???
}
