import cats.effect.{IO, Ref}
import fs2.Stream
import cats.implicits.*
import ch10_CastingDieConcurrently.castTheDie

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

object ExerciseOnChapter10 {
  opaque type City = String

  private object City {
    def apply(name: String): City = name

    extension (city: City) def name: String = city
  }

  private case class CityStats(city: City, checkIns: Int)

  val checkIns: Stream[IO, City] =
    Stream(
      City("Sydney"),
      City("Dublin"),
      City("Cape Town"),
      City("Lima"),
      City("Singapore"),
    )
      .repeatN(100_000)
      .append(Stream.range(0, 100_000).map(i => City(s"City $i")))
      .append(
        Stream(
          City("Sydney"),
          City("Sydney"),
          City("Lima")
        )
      )
      .covary[IO]

  def processCheckIn(checkIns: Stream[IO, City]): IO[Unit] =
    checkIns
      .scan(Map.empty[City, Int])((cityCheckIns, city) =>
        // Some なら value + 1, None なら 1
        // patten match するよりも簡潔！
        cityCheckIns.updatedWith(city)(_.map(_ + 1).orElse(Some(1)))
      )
      .chunkN(100_000)
      .map(_.last)
      .unNone
      .map(topCities)
      .foreach(IO.println)
      .compile
      .drain

  // 意味的に明らかな名前を使うとわかりやすい
  private def topCities(cityCheckIns: Map[City, Int]): List[CityStats] =
    cityCheckIns
      .toList
      .map((city, checkIns) => CityStats(city, checkIns))
      .sortBy(_.checkIns)
      .reverse
      .take(3)
}

object Exercise1017 {
  val p1: IO[Int] = for {
    _ <- IO.sleep(FiniteDuration(1, TimeUnit.SECONDS))
    result <- List.fill(2)(castTheDie()).sequence.map(_.sum)
  } yield result

  val p2: IO[Ref[IO, List[Int]]] = for {
    result <- List.fill(2)(castTheDie()).parSequence
    ref <- Ref.of[IO, List[Int]](result)
  } yield ref

  val p3: IO[Ref[IO, List[Int]]] = for {
    result <- List.fill(3)(castTheDie()).parSequence
    ref <- Ref.of[IO, List[Int]](result)
  } yield ref

  val p4: IO[Ref[IO, Int]] = for {
    result <- List.fill(100)(castTheDie()).parSequence.map(_.count(_ == 6))
    ref <- Ref.of[IO, Int](result)
  } yield ref

  val p5: IO[Int] = for {
    result <- List.fill(100)(
      IO.sleep(FiniteDuration(1, TimeUnit.SECONDS)).flatMap(_ => castTheDie())
    ).parSequence
  } yield result.sum
}
