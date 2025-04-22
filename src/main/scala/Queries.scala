object Queries {

  def killJackSparrow(t: Table): Option[Table] = queryT((Some(t), "FILTER", Not(Field("name", _ == "Jack"))))

  def stringToTab(s: String): Map[String, String] = {
    s.split(",").map { pair =>
      val Array(k, v) = pair.trim.split(" ", 2)
      (k.trim, v.trim)
    }.toMap
  }

  val list: Tabular = List(stringToTab("name Ana, age 93, CNP 455550555"), stringToTab("name Diana, age 33, CNP 255532142"), stringToTab("name Tatiana, age 55, CNP 655532132"), stringToTab("name Rosmaria, age 12, CNP 855532172"))

  def insertLinesThenSort(db: Database): Option[Table] = queryT(queryT(Some(queryDB(queryDB(Some(db), "CREATE", "Inserted Fellas"), "EXTRACT", List("Inserted Fellas")).get.tables.head), "INSERT", list), "SORT", "age")

  def youngAdultHobbiesJ(db: Database): Option[Table] =   queryT(queryT((queryT(queryT(Some(queryDB(queryDB(Some(db), "JOIN", "People", "name", "Hobbies", "name"), "SELECT", List("Joined")).get.tables.head), "FILTER", Field("name", _.head == 'J')), "FILTER", Field("age", _ < "25")), "FILTER", Not(Field("hobby", _ == " ")))), "EXTRACT", List("name", "hobby"))

}
