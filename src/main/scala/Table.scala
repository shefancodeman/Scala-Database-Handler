import scala.annotation.tailrec

type Row = Map[String, String]
type Tabular = List[Row]

case class Table (tableName: String, tableData: Tabular) {
  override def toString: String = if (tableData.nonEmpty){
    header.mkString(",") + "\n" + data.map { row =>
      header.map(key => row(key)).mkString(",")
    }.mkString("\n")
  } else "Empty"

  def insert(row: Row): Table = if(tableData.contains(row)) Table(tableName, tableData) else Table(tableName,(tableData :+ row).sortBy(_(header.head).toInt))

  def delete(row: Row): Table = Table(tableName, tableData.filter(_ != row))

  def sort(column: String): Table = Table(tableName, tableData.sortBy(_.getOrElse(column, "")))

  def update(f: FilterCond, updates: Map[String, String]): Table = Table(tableName, tableData.map { row =>
    if (f.eval(row).getOrElse(false)) {
      row.map { case (key, value) =>
        updates.get(key) match {
          case Some(newValue) => key -> newValue
          case None => key -> value
        }
      }
    } else {
      row
    }
  })

  def filter(f: FilterCond): Table = Table(tableName, tableData.filter(f.eval(_).get))

  def select(columns: List[String]): Table = Table(tableName, tableData.map(row => row.filterKeys(columns.contains).toMap))

  def header: List[String] = tableData.head.keys.toList
  def data: Tabular = tableData
  def name: String = tableName
}

object Table {
  def apply(name: String, s: String): Table = {
    val header = s.split("\n").head.split(",").map(_.trim)
    Table(name, s.split("\n").tail.toList.map(line => header.zip(line.split(",").map(_.trim)).toMap))
  }
}

extension (table: Table) {
  def todo(i: Int): Table = Table(table.tableName, table.tableData.drop(i - 1).take(1))
}
