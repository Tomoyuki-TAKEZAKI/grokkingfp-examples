import cats.effect.IO
import fs2.Stream

object ExerciseOnChapter10 {
  opaque type City = String

  object City {
    def apply(name: String): City = name

    extension (city: City) def name: String = city
  }

  case class CityStats(city: City, checkIns: Int)

  val checkIns: Stream[IO, City] =
    Stream(
      City("Sydney"),
      City("Sydney"),
      City("Cape Town"),
      City("Singapore"),
      City("Cape Town"),
      City("Sydney"),
    ).covary[IO]
    
  def processCheckIn(checkIns: Stream[IO, City]) : IO[Unit] = ???

}
