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
  def f01[A, B](x: IO[A], f: A => B): IO[B] = x.map(f)

  def f02[A](x: IO[IO[A]]): IO[A] = x.flatten

  def f03[A, B](x: IO[A], f: A => IO[B]): IO[B] = x.flatMap(f)

  def f04[A](x: A): IO[A] = IO.pure(x)

  def f05[A](impureAction: () => A): IO[A] = IO.delay(impureAction())

  def f06[A](x: IO[A], alternative: IO[A]): IO[A] = x.orElse(alternative)

  def f07[A](x: List[IO[A]]): IO[List[A]] = x.sequence

  def f08[A](x: Option[IO[A]]): IO[Option[A]] = x.sequence

  def f09[A, B](x: List[A], y: List[A]): List[A] = x.appendedAll(y)

  def f10[A](x: List[A], f: A => Boolean): List[A] = x.filter(f)

  def f11[A](x: List[A], zero: A, f: (A, A) => A): A = x.foldLeft(zero)(f)

  def f12[A](x: List[List[A]]): List[A] = x.flatten

  def f13[A, B](x: List[A], f: A => List[B]): List[B] = x.flatMap(f)

  def f14[A](x: List[A], f: A => Boolean): Boolean = x.forall(f)

  def f15[A, B](x: Set[A], f: A => B): Set[B] = x.map(f)

  def f16[A](x: Set[A], f: A => Boolean): Set[A] = x.filter(f)

  def f17[A](x: Set[A], zero: A, f: (A, A) => A): A = x.foldLeft(zero)(f)

  def f18[A](x: Set[Set[A]]): Set[A] = x.flatten

  def f19[A, B](x: Set[A], f: A => Set[B]): Set[B] = x.flatMap(f)

  def f20[A](x: Set[A], f: A => Boolean): Boolean = x.forall(f)

  def f21[A, B](x: Option[A], f: A => B): Option[B] = x.map(f)

  def f22[A](x: Option[A], f: A => Boolean): Option[A] = x.filter(f)

  def f23[A](x: Option[A], zero: A, f: (A, A) => A): A = x.foldLeft(zero)(f)

  def f24[A](x: Option[Option[A]]): Option[A] = x.flatten

  def f25[A, B](x: Option[A], f: A => Option[B]): Option[B] = x.flatMap(f)

  def f26[A](x: Option[A], f: A => Boolean): Boolean = x.forall(f)

  def f27(x: String): Option[Int] = x.toIntOption

  def f28[A](x: Option[A], alternative: Option[A]): Option[A] = x.orElse(alternative)

  def f29[A, B](x: Option[A], y: B): Either[B, A] = x.toRight(y)

  def f30[A, B](x: Option[A], y: B): Either[A, B] = x.toLeft(y)

  def f31[A](x: List[Option[A]]): Option[List[A]] = x.sequence

  def f32[A, B, C](x: Either[A, B], f: B => C): Either[A, C] = x.map(f)

  def f33[A, B, C](x: Either[A, B], zero: C, f: (C, B) => C): C = x.foldLeft(zero)(f)

  def f34[A, B](x: Either[A, Either[A, B]]): Either[A, B] = x.flatten

  def f35[A, B, C](x: Either[A, B], f: B => Either[A, C]): Either[A, C] = x.flatMap(f)

  def f36[A, B](x: Either[A, B], f: B => Boolean): Boolean = x.forall(f)

  def f37[A, B](x: Either[A, B], alternative: Either[A, B]): Either[A, B] = x.orElse(alternative)

  def f38[A, B](x: Either[A, B]): Option[B] = x.toOption

  def f39[A, B](x: List[Either[A, B]]): Either[A, List[B]] = x.sequence

  def f40[A, B](x: Either[A, List[B]]): List[Either[A, B]] = x.sequence
}
