import scala.language.postfixOps

case class Database(tables: List[Table]) {
  override def toString: String = tables.map(_.toString).mkString("\n\n")

  def create(tableName: String): Database = {
    if(tables.exists(_.tableName == tableName))
      this
    else
      Database(tables :+ Table(tableName, List.empty[Row]))
  }

  def drop(tableName: String): Database = Database(tables.filterNot(_.tableName == tableName))

  def selectTables(tableNames: List[String]): Option[Database] = {
    if (tableNames.flatMap(name => tables.find(_.tableName == name)).length == tableNames.length) {
      Some(Database(tableNames.flatMap(name => tables.find(_.tableName == name))))
    } else None
  }

  def join(table1: String, c1: String, table2: String, c2: String): Option[Table] = {
    def merge(row1: Map[String, String], row2: Map[String, String]): Map[String, String] = {
      row1.keys.map { key =>
        val newval = (row1.get(key), row2.get(key)) match {
          case (Some(v1), Some(v2)) if (v1 == "" && v2 != "") => v2
          case (Some(v1), Some(v2)) if (v1 != "" && v2 == "") => v1
          case (Some(v1), Some(v2)) if (v1 == v2) => v1
          case (Some(v1), Some(v2)) if (v1 != v2) => s"$v1;$v2"
          case (Some(v1), _) => v1
          case (_, Some(v2)) => v2
          case _ => ""
        }
        key -> newval
      }.toMap
    }

    if (!tables.exists(_.name == table1) || !tables.exists(_.name == table2))
      None
    else {
      val A = tables.find(_.name == table1).get
      val B = tables.find(_.name == table2).get
      if (A.header.contains(c1) && B.header.contains(c2)) {
        val toAdd = B.header.filterNot(A.header.intersect(B.header) contains).filterNot(_ == c2)
        val jointab = A.tableData.map { map => toAdd.foldLeft(map)((acc, word) => acc + (word -> "")) }
        val result = jointab.map { row =>
          B.tableData.find(_(c2) == row(c1)) match {
            case Some(found) => merge(row, found)
            case None => row
          }
        }.sortBy(_.values.count(_ == ""))
        val listB: List[Map[String, String]] = B.tableData.flatMap { row =>
          if (!A.tableData.map(_(c1)).contains(row(c2))) {
            val row_toAdd = (A.header ++ toAdd).map(key => key -> "").toMap
            List(row_toAdd.map { case (key, value) =>
              if (row.contains(key)) key -> row(key)
              else key -> value
            } + (c1 -> row(c2)))
          } else {
            None
          }
        }
        Some(Table("Joined", result ++ listB))
      }
      else
        None
    }
  }

  def apply(index: Int): Table = tables(index)
}
