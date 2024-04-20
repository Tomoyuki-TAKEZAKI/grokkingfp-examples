import cats.effect.IO
import fs2.Stream

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
      City("Sydney"),
      City("Cape Town"),
      City("Singapore"),
      City("Cape Town"),
      City("Sydney"),
    ).covary[IO]

  def processCheckIn(checkIns: Stream[IO, City]): IO[Unit] =
    checkIns
      .scan(Map.empty[City, Int])((cityCheckIns, city) =>
        // Some なら value + 1, None なら 1
        // patten match するよりも簡潔！
        cityCheckIns.updatedWith(city)(_.map(_ + 1).orElse(Some(1)))
      )
      .map(topCities)
//      .foreach(cityStats => IO.delay(println(cityStats)))
      .foreach(IO.println) // 上記のコードより簡潔な表現！
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
