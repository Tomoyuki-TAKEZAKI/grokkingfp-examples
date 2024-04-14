import cats.effect.IO
import ch09_CurrencyExchange.exchangeRatesTableApiCall
import ch08_SchedulingMeetings.retry

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

  def exchangeIfTrending(
    amount: BigDecimal,
    from: Currency,
    to: Currency,
  ): IO[BigDecimal] =
    for {
      rates <- lastRates(from, to, 3)
      result <- if (trending(rates)) IO.pure(amount * rates.last)
      else exchangeIfTrending(amount, from, to)
    } yield result
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
