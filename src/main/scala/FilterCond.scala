import scala.language.implicitConversions

trait FilterCond {def eval(r: Row): Option[Boolean]}

case class Field(colName: String, predicate: String => Boolean) extends FilterCond {
  override def eval(r: Row): Option[Boolean] =  r.get(colName).map(predicate)
}

case class Compound(op: (Boolean, Boolean) => Boolean, conditions: List[FilterCond]) extends FilterCond {
  override def eval(r: Row): Option[Boolean] =
    val res = conditions.map(_.eval(r))
    if (res.forall(_.isDefined)) {
      Some(res.map(_.get).reduce(op))
    } else {
      None
    }
}
case class Not(f: FilterCond) extends FilterCond {
  override def eval(r: Row): Option[Boolean] = f.eval(r) match
    case Some(value) => Some(!value)
    case None => None
}

def And(f1: FilterCond, f2: FilterCond): FilterCond = (r: Row) => {
  if (f2.eval(r).get && f1.eval(r).get)
    Some(true)
  else
    Some(false)
}
def Or(f1: FilterCond, f2: FilterCond): FilterCond = (r: Row) => {
  if (f2.eval(r).get || f1.eval(r).get)
    Some(true)
  else
    Some(false)
}
def Equal(f1: FilterCond, f2: FilterCond): FilterCond = (r: Row) => {
  if (f2.eval(r).get == f1.eval(r).get)
    Some(true)
  else
    Some(false)
}

case class Any(fs: List[FilterCond]) extends FilterCond {
  override def eval(r: Row): Option[Boolean] =  Some(fs.exists(_.eval(r).contains(true)))
}

case class All(fs: List[FilterCond]) extends FilterCond {
  override def eval(r: Row): Option[Boolean] = Some(!fs.exists(_.eval(r).contains(false)))
}

implicit def tuple2Field(t: (String, String => Boolean)): Field =  Field(t._1, t._2)

extension (f: FilterCond) {
  def ==(other: FilterCond) = Equal(f, other)
  def ===(other: (String, String => Boolean)): FilterCond = Equal(f, other)
  def &&(other: FilterCond) = And(f, other)
  def ||(other: FilterCond) = Or(f, other)
  def !! = Not(f)
}