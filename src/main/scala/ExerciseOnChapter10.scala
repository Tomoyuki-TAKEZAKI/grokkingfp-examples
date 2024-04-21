import cats.effect.{IO, Ref}
import fs2.Stream
import cats.implicits.*
import ch10_CastingDieConcurrently.castTheDie
import scala.concurrent.duration._

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
    _ <- IO.sleep(1.seconds)
    result <- List.fill(2)(castTheDie()).sequence.map(_.sum)
  } yield result

  // ここでは Ref は中間結果を安全に保管するために利用して、結果としては List 型を返せというのが題意であった。
  val p2: IO[List[Int]] = for {
    ref <- Ref.of[IO, List[Int]](List.empty)
    singleCast = castTheDie().flatMap(result => ref.update(_.appended(result)))
    _ <- List(singleCast, singleCast).parSequence
    casts <- ref.get
  } yield casts

  // ここでは Ref は中間結果を安全に保管するために利用して、結果としては List 型を返せというのが題意であった。
  val p3: IO[List[Int]] = for {
    ref <- Ref.of[IO, List[Int]](List.empty)
    singleCast = castTheDie().flatMap(result => ref.update(_.appended(result)))
    _ <- List.fill(3)(singleCast).parSequence
    casts <- ref.get
  } yield casts

  // ここでは Ref は中間結果を安全に保管するために利用して、結果としては List 型を返せというのが題意であった。
  val p4: IO[Int] = for {
    ref <- Ref.of[IO, Int](0)
    singleCast = castTheDie().flatMap(result =>
      if (result == 6) {
        ref.update(_ + 1)
      } else {
        IO.unit
      }
    )
    result <- List.fill(100)(singleCast).parSequence
    count <- ref.get
  } yield count

  val p5: IO[Int] = List.fill(100)(
    IO.sleep(1.seconds).flatMap(_ => castTheDie())
  ).parSequence.map(_.sum)
}
