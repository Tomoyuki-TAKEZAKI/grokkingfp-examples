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

  def lastRates(from: Currency, to: Currency): IO[List[BigDecimal]] =
    for {
      table1 <- retry(exchangeTable(from), 10)
      table2 <- retry(exchangeTable(from), 10)
      table3 <- retry(exchangeTable(from), 10)
      lastTables = List(table1, table2, table3)
    } yield lastTables.flatMap(extractSingleCurrencyRate(to))

  def exchangeIfTrending(
    amount: BigDecimal,
    from: Currency,
    to: Currency,
  ): IO[Option[BigDecimal]] =
    lastRates(from, to).map(rates =>
      if (trending(rates)) Some(amount * rates.last) else None
    )
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
