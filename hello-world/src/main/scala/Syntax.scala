package hbir

import scala.util.parsing.input.{Positional}

object Syntax {
  case class ConfigSection(arrangements : List[Arrangement])

  case class Arrangement(
    gridName: Id, 
    phys_limit_bindings: (Id, Id),
    groups: List[Group])
  case class Group(name: Id, mapping: CoordMap)
  case class CoordMap(domain: List[(Id, Expr)], range: List[Slice])


  case class CodeSection(cmd: Command)

  trait Command
  case class CmdInitDecl(typ: Type, id: Id, rhs: Expr) extends Command
  case class CmdDecl(typ: Type, ids: List[Id]) extends Command
  case class CmdPrint(id: Id) extends Command
  case class CmdRunKernel(kernel: Id, args: List[Id]) extends Command
  case class CmdSeq(c1: Command, c2: Command) extends Command
  case class CmdAssign(id: Id, rhs: Expr) extends Command
  case class CmdProcCall(proc: Id, args: List[Id]) extends Command

  trait Type
  case object TInt extends Type
  case class TTensor(typ: Type, size: List[Expr]) extends Type

  trait Expr 
  case class EVar(v: Id) extends Expr
  case class EInt(v: Int) extends Expr
  case class EBinop(op: BOp, e1: Expr, e2: Expr) extends Expr

  sealed trait BOp {
    val op: String;
    override def toString = this.op;
  }

  case class NumOp(op: String) extends BOp

  case class Slice(start: Expr, end: Expr)

  case class Id(v: String) extends Positional
}
