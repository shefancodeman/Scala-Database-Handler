import scala.language.implicitConversions

trait PP_SQL_DB{
  def eval: Option[Database]
}

case class CreateTable(database: Database, tableName: String) extends PP_SQL_DB{
  def eval: Option[Database] = Some(database.create(tableName))
}

case class DropTable(database: Database, tableName: String) extends PP_SQL_DB{
  def eval: Option[Database] = Some(database.drop(tableName))
}

implicit def PP_SQL_DB_Create_Drop(t: (Option[Database], String, String)): Option[PP_SQL_DB] = t match {
  case (Some(database), "CREATE", tableName) => Some(CreateTable(database, tableName))
  case (Some(database), "DROP", tableName) => Some(DropTable(database, tableName))
}

case class SelectTables(database: Database, tableNames: List[String]) extends PP_SQL_DB{
  def eval: Option[Database] = database.selectTables(tableNames)
}

implicit def PP_SQL_DB_Select(t: (Option[Database], String, List[String])): Option[PP_SQL_DB] = t match {
  case (Some(database), "EXTRACT", tableNames) => Some(SelectTables(database, tableNames))
  case (Some(database), "SELECT", tableNames) => Some(SelectTables(database, tableNames))
  case _ => None
}

case class JoinTables(database: Database, table1: String, column1: String, table2: String, column2: String) extends PP_SQL_DB{
  def eval: Option[Database] = Some(Database(List(database.join(table1, column1, table2, column2).get)))
}

implicit def PP_SQL_DB_Join(t: (Option[Database], String, String, String, String, String)): Option[PP_SQL_DB] = t match {
  case (Some(database), "JOIN", table1, column1, table2, column2) => Some(JoinTables(database, table1, column1, table2, column2))
  case _ => None
}

trait PP_SQL_Table{
  def eval: Option[Table]
}

case class InsertRow(table:Table, values: Tabular) extends PP_SQL_Table{
  def eval: Option[Table] = Some(Table(table.tableName, table.tableData ++ values))
}

implicit def PP_SQL_Table_Insert(t: (Option[Table], String, Tabular)): Option[PP_SQL_Table] = t match {
  case (Some(table), "INSERT", values) => Some(InsertRow(table, values))
  case _ => None
}

case class UpdateRow(table: Table, condition: FilterCond, updates: Map[String, String]) extends PP_SQL_Table{
  def eval: Option[Table] = Some(table.update(condition, updates))
}

implicit def PP_SQL_Table_Update(t: (Option[Table], String, FilterCond, Map[String, String])): Option[PP_SQL_Table] = t match {
  case (Some(table), "UPDATE", condition, updates) => Some(UpdateRow(table, condition, updates))
  case _ => None
}

case class SortTable(table: Table, column: String) extends PP_SQL_Table{
  def eval: Option[Table] = Some(table.sort(column))
}

implicit def PP_SQL_Table_Sort(t: (Option[Table], String, String)): Option[PP_SQL_Table] = t match {
  case (Some(table), "SORT", column) => Some(SortTable(table, column))
  case _ => None
}

case class DeleteRow(table: Table, row: Row) extends PP_SQL_Table{
  def eval: Option[Table] = Some(table.delete(row))
}

implicit def PP_SQL_Table_Delete(t: (Option[Table], String, Row)): Option[PP_SQL_Table] = t match {
  case (Some(table), "DELETE", row) => Some(DeleteRow(table, row))
  case _ => None
}

case class FilterRows(table: Table, condition: FilterCond) extends PP_SQL_Table{
  def eval: Option[Table] = Some(table.filter(condition))
}

implicit def PP_SQL_Table_Filter(t: (Option[Table], String, FilterCond)): Option[PP_SQL_Table] = t match {
  case (Some(table), "FILTER", condition) => Some(FilterRows(table, condition))
  case _ => None
}

case class SelectColumns(table: Table, columns: List[String]) extends PP_SQL_Table{
  def eval: Option[Table] = Some(table.select(columns))
}

implicit def PP_SQL_Table_Select(t: (Option[Table], String, List[String])): Option[PP_SQL_Table] = t match {
  case (Some(table), "EXTRACT", columns) => Some(SelectColumns(table, columns))
  case (Some(table), "SELECT", columns) => Some(SelectColumns(table, columns))
  case _ => None
}

def queryT(p: Option[PP_SQL_Table]): Option[Table] = p.flatMap(_.eval)
def queryDB(p: Option[PP_SQL_DB]): Option[Database] = p.flatMap(_.eval)