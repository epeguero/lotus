package hbir

import scala.util.parsing.input.{Positional}

object Syntax {
  case class ConfigSection(arrangements : List[Arrangement])

  case class Arrangement(
    gridName: Id, 
    physLimitBindings: (Id, Id),
    groups: List[Group])
  case class Group(name: Id, mapping: CoordMap)
  case class CoordMap(domain: List[(Id, Expr)], range: List[Slice])

  case class KernelSection(kernels: List[Kernel])
  case class Kernel(name: Id, inputs: List[(Id, Type)], out: (Id, Type), partition: KernelPartition, body: KernelStatement)
  case class KernelPartition(strategy: Id, tensors: List[Id], group: Id, gidName: List[Id])

  trait KernelStatement
  case class KStream(source: List[SpecifiedTensor], target: List[SpecifiedTensor], stmt: KernelStatement) extends KernelStatement
  case class KVvaddStore(out: SpecifiedTensor, v1: SpecifiedTensor, v2: SpecifiedTensor) extends KernelStatement
  case class SpecifiedTensor(name: Id, gid: List[Id], memLoc: Id)

  case class CodeSection(cmd: Option[Command])

  trait Command
  case class CmdInitDecl(typ: Type, id: Id, rhs: Expr) extends Command
  case class CmdDecl(typ: Type, ids: List[Id]) extends Command
  case class CmdPrint(id: Id) extends Command
  case class CmdRunKernel(kernel: Id, args: List[Expr]) extends Command
  case class CmdSeq(c1: Command, c2: Command) extends Command
  case class CmdAssign(id: Id, rhs: Expr) extends Command
  case class CmdProcCall(proc: Id, args: List[Expr]) extends Command

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
  case class Param(name: Id, typ: Type)

  implicit def toString(id: Id) = id match {
    case Id(s) => s
  }
}
