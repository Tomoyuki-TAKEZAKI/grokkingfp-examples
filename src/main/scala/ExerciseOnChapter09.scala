import cats.effect.IO
import ch09_CurrencyExchange.exchangeRatesTableApiCall
import ch08_SchedulingMeetings.retry
import fs2.Stream
import ch08_CastingDie.castTheDieImpure
import cats.implicits._

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

object ExerciseOnChapter09 {

  opaque type Currency = String

  object Currency {
    def apply(name: String): Currency = name

    extension (currency: Currency) def name: String = currency
  }

  def trending(rates: List[BigDecimal]): Boolean =
    rates.size > 1 &&
      rates.zip(rates.drop(1))
        .forall((previousRate, rate) => previousRate < rate)

  def extractSingleCurrencyRate(currencyToExtract: Currency)
    (table: Map[Currency, BigDecimal]): Option[BigDecimal] =
    table.get(currencyToExtract)

  def exchangeTableApiCall(currency: String): Map[String, BigDecimal] =
    exchangeRatesTableApiCall(currency)

  def exchangeTable(from: Currency): IO[Map[Currency, BigDecimal]] =
    IO.delay(
      exchangeTableApiCall(from).map(
        (currencyName, rate) => (Currency(currencyName), rate)
      )
    )

  def lastRates(from: Currency, to: Currency, n: Int): IO[List[BigDecimal]] =
    if (n < 1) {
      IO.pure(List.empty)
    } else {
      for {
        currencyRate <- currencyRate(from, to)
        remainingRates <- if (n == 1) IO.pure(List.empty) else lastRates(from, to, n - 1)
      } yield remainingRates.prepended(currencyRate)
    }

  def currencyRate(from: Currency, to: Currency): IO[BigDecimal] =
    for {
      table <- retry(exchangeTable(from), 10)
      result <- extractSingleCurrencyRate(to)(table) match
        case Some(value) => IO.pure(value)
        case None => currencyRate(from, to)
    } yield result

  def rates(from: Currency, to: Currency): Stream[IO, BigDecimal] =
    Stream
      .eval(exchangeTable(from))
      .repeat
      .map(extractSingleCurrencyRate(to))
      .unNone
      .orElse(rates(from, to)) // import cats.implicits._ が必要

  val delay: FiniteDuration = FiniteDuration(1, TimeUnit.SECONDS)
  val ticks: Stream[IO, Unit] = Stream.fixedRate[IO](delay)

  def exchangeIfTrending(
    amount: BigDecimal,
    from: Currency,
    to: Currency,
  ): IO[BigDecimal] =
    rates(from, to)
      .zipLeft(ticks)
      .sliding(3)
      .map(_.toList)
      .filter(trending)
      .map(_.last)
      .head
      .compile
      .lastOrError
      .map(_ * amount)
}

object Exercise0906 {
  val m1 = Map(
    "key" -> "value"
  )

  val m2 = m1.updated("key2", "value2")

  val m3 = m2.updated("key2", "another2")

  val m4 = m3.removed("key")

  val valueFromM3: Option[String] = m3.get("key")

  val valueFromM4: Option[String] = m4.get("key")
}

object Exercise0940 {
  def castTheDie(): IO[Int] = IO.delay(castTheDieImpure())

  val infiniteDieCasts: Stream[IO, Int] = Stream.eval(castTheDie()).repeat

  val s1: IO[List[Int]] = infiniteDieCasts.filter(_ % 2 != 0).take(3).compile.toList

  val s2: IO[List[Int]] = infiniteDieCasts.take(5).map(n => if (n == 6) n * 2 else n).compile.toList

  val s3: IO[List[Int]] = infiniteDieCasts.take(3).fold(0)(_ + _).compile.toList // fold せずに結果をマップしてたしても良い

  val s3_ : IO[List[Int]] = infiniteDieCasts.take(3).scan(0)((acc, n) => acc + n).compile.toList.map(_.drop(3)) // scan 使ってみたがスマートなやり方がありそうだ

  val s4: IO[List[Int]] = infiniteDieCasts.filter(_ == 5).take(1).append(infiniteDieCasts.take(2)).compile.toList

  // 問題4で、最後の3つだけではなく全ての目を返す場合
  val s4_ : IO[List[Int]] = infiniteDieCasts.takeThrough(_ != 5).append(infiniteDieCasts.take(2)).compile.toList

  val s5: IO[Unit] = infiniteDieCasts.take(100).compile.drain

  val s6: IO[List[Int]] = infiniteDieCasts.take(3).append(infiniteDieCasts.take(3).map(_ * 3)).compile.toList

  val s7: IO[List[(Int, Int)]] = infiniteDieCasts
    .scan((0, 0))((acc, current) => (acc._2, current))
    .filter((prev, current) => prev == 6 && current == 6)
    .take(1).compile.toList
  // 解答例のように、 acc の値を current が 6 なら acc + 1, 6 以外なら 0 にする実装でも良い。
}