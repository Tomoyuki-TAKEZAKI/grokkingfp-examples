package learn

object ExerciseOnChapter09 {

  opaque type Currency = String

  object Currency {
    def apply(name: String): Currency = name
    extension (currency: Currency) def name: String = currency
  }
}

object Exercise0906 {
  val m1 = Map(
    "key" -> "value"
  )

  val m2 = m1.updated("key2","value2")

  val m3 = m2.updated("key2", "another2")

  val m4 = m3.removed("key")

  val valueFromM3: Option[String] = m3.get("key")

  val valueFromM4: Option[String] = m4.get("key")
}
