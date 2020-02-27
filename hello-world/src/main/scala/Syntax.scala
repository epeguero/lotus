package hbir

import scala.util.parsing.input.{Positional, Position}

object Syntax {
  case class ConfigSection(arrangements : List[Arrangement]) extends Positional

  case class Arrangement(
    gridName: Id, 
    phys_limit_bindings: (Id, Id),
    groups: List[Group]) extends Positional
  case class Group(name: Id, mapping: CoordMap) extends Positional
  case class CoordMap(domain: List[(Id, Expr)], range: List[Slice]) extends Positional

  sealed trait Expr extends Positional
  case class EVar(v: Id) extends Expr
  case class EInt(v: Int) extends Expr
  case class EBinop(op: BOp, e1: Expr, e2: Expr) extends Expr

  sealed trait BOp extends Positional {
    val op: String;
    override def toString = this.op;
  }

  case class NumOp(op: String) extends BOp

  case class Slice(start: Expr, end: Expr) extends Positional

  case class Id(v: String) extends Positional
}
