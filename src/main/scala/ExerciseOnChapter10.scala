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
    scanCheckIn(checkIns)
      .map(convertToCityStats)
      .map(sortCityStats)
      .take(3)
      .foreach(cityStats => IO.delay(println(cityStats)))
      .compile
      .drain

  private def scanCheckIn(checkIns: Stream[IO, City]): Stream[IO, Map[City, Int]] =
    checkIns
      .scan(Map.empty[City, Int])((map, city) =>
        map.updatedWith(city) {
          case Some(count) => Some(count + 1)
          case None => Some(1)
        }
      )

  private def convertToCityStats(map: Map[City, Int]): List[CityStats] =
    map.toList.map((city, count) => CityStats(city, count))

  private def sortCityStats(list: List[CityStats]): List[CityStats] =
    list.sortBy(_.checkIns).reverse
}
