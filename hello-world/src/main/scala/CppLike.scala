package hbir

import Syntax._
import PrettyPrint.Doc._
import PrettyPrint.Doc

object CLike {

    /**
      * This class aggressively uses Scala's implicitConversions. Make sure
      * that implicits classes never leak.
      * Implicit classes: https://docs.scala-lang.org/tour/implicit-conversions.html
      *
      */
    import scala.language.implicitConversions

    val defaultIndent = 2

    val cInt = text("int") 

    /**
      * Helper to generate a variable declaration with an initial value.
      */
    def cBind(id: Doc, rhs: Doc): Doc = {
      id <+> text("=") <+> rhs <> semi
    }

    def cDeclInit(typ: Doc, id: Doc, rhs: Doc): Doc = 
      typ <+> id <+> text("=") <+> rhs <> semi
    

    /**
      * Helper to generate a function call that might have a type parameter
      */
    def cCall(f: String, args: List[Doc]): Doc = {
      text(f) <> parens(commaSep(args))
    }

    def cFunDef(f: String, params: List[Doc], body: Doc) = {
      text(f) <+> parens(commaSep(params)) <+> braces(nest(emptyDoc <@> body, 1))
    }


    /**
      * Function used for converting types from Fuse to C++.
      */
     /*
    def emitType(typ: Type): Doc
     */
    /**
      * Used for generating declarations for array. For example, an array
      * parameter `arr` in a function can be passed as `*arr` or `arr[size]`.
      */
    // def emitarraydecl(typ: tarray, id: id): doc

    /**
      * used to emit for loops. we specifically separate this out from other
      * commands since for loops might require pragmas.
      */
    // def emitfor(cmd: cfor): doc

    /**
      * emit while loops.
      */
    // def emitwhile(cmd: cwhile): doc =
    //   text("while") <> parens(cmd.cond) <+> scope(cmd.body)

    /**
      * used to emit function headers. all c++ backends will convert the function
      * bodies in the same way but might require different pragrams for arguments
      * or setup code. `entry` distinguishes the top-level entry point
      * function.
      */
    // def emitfuncheader(func: funcdef, entry: boolean = false): doc

    /**
      * Generate code for a "let" binding. (Might need to be followed by
      * pragmas.)
      */
    // def emitLet(let: CLet): Doc =
    //   emitDecl(let.id, let.typ.get) <>
    //     (if (let.e.isDefined) space <> equal <+> emitExpr(let.e.get)
    //      else emptyDoc) <>
    //     semi

    implicit def IdToString(id: Id): Doc = value(id.v)

    // def emitBaseInt(v: Int, base: Int): String = base match {
    //   case 8  => s"0${Integer.toString(v, 8)}"
    //   case 10 => v.toString
    //   case 16 => s"0x${Integer.toString(v, 16)}"
    // }

    implicit def emitExpr(e: Expr): Doc = text("some Expr, boii")
      /*
      e match {
      case ECast(e, typ)      => parens(emitType(typ)) <> emitExpr(e)
      case EApp(fn, args)     => fn <> parens(commaSep(args.map(emitExpr)))
      case EInt(v, base)      => value(emitBaseInt(v, base))
      case ERational(d)       => value(d)
      case EBool(b)           => value(if (b) 1 else 0)
      case EVar(id)           => value(id)
      case EBinop(op, e1, e2) => parens(e1 <+> text(op.toString) <+> e2)
      case EArrAccess(id, idxs) =>
        id <> ssep(idxs.map(idx => brackets(emitExpr(idx))), emptyDoc)
      case EArrLiteral(idxs)      => braces(commaSep(idxs.map(idx => emitExpr(idx))))
      case ERecAccess(rec, field) => rec <> dot <> field
      case ERecLiteral(fs) =>
        scope {
          commaSep(fs.toList.map({
            case (id, expr) => dot <> id <+> equal <+> expr
          }))
        }
    }
        */

    /**
      * Turns a range object into the parameter of a `for` loop.
      * (int <id> = <s>; <id> < <e>; <id>++)
      */
    /*
    def emitRange(range: CRange): Doc = parens {
      text("int") <+> range.iter <+> equal <+> value(range.s) <> semi <+>
        range.iter <+> text("<") <+> value(range.e) <> semi <+>
        range.iter <> text("++")
    }

    implicit def emitCmd(c: Command): Doc = c match {
      case CPar(c1, c2) => c1 <@> c2
      case CSeq(c1, c2) => c1 <@> text("//---") <@> c2
      case l: CLet      => emitLet(l)
      case CIf(cond, cons, alt) => {
        text("if") <+> parens(cond) <+> scope(cons) <> (alt match {
          case CEmpty => emptyDoc
          case _      => space <> text("else") <+> scope(alt)
        })
      }
      case f: CFor                => emitFor(f)
      case w: CWhile              => emitWhile(w)
      case CDecorate(dec)         => value(dec)
      case CUpdate(lhs, rhs)      => lhs <+> equal <+> rhs <> semi
      case CReduce(rop, lhs, rhs) => lhs <+> text(rop.toString) <+> rhs <> semi
      case CReturn(e)             => text("return") <+> e <> semi
      case CExpr(e)               => e <> semi
      case CEmpty                 => emptyDoc
      case _: CView | _: CSplit =>
        throw Impossible("Views should not exist during codegen.")
    }

    def emitDecl(id: Id, typ: Type): Doc = typ match {
      case ta: TArray => emitArrayDecl(ta, id)
      case _          => emitType(typ) <+> id
    }

    def emitFunc(func: FuncDef, entry: Boolean = false): Doc = func match {
      case func @ FuncDef(id, args, ret, bodyOpt) =>
        val as = commaSep(args.map(decl => emitDecl(decl.id, decl.typ)))
        // If body is not defined, this is an extern. Elide the definition.
        val body = bodyOpt
          .map(
            body =>
              emitType(ret) <+> id <> parens(as) <+>
                scope { emitFuncHeader(func, entry) <@> body }
          )
          .getOrElse(emptyDoc)

        if (entry) text("extern") <+> quote(text("C")) <+> scope(body)
        else body
    }

    def emitDef(defi: Definition): Doc = defi match {
      case func: FuncDef => emitFunc(func)
      case RecordDef(name, fields) =>
        text("typedef struct") <+> scope {
          vsep(fields.toList.map({
            case (id, typ) => emitType(typ) <+> id <> semi
          }))
        } <+> name <> semi
    }

    def emitInclude(incl: Include): Doc =
      text("#include") <+> quote(text(incl.name))

  */
}

object Gem5Backend {
  import CLike._
  import PrettyPrint.Doc._
  import Syntax._

  def main(cs: CodeSection) : Doc = cs match { 
    case CodeSection(cmd) => cFunDef("main", List(), cStmt(cmd))
  }
 
  def cType(typ: Type) : Doc = typ match {
    case TInt => cInt
    case TTensor(typ, size) => cType(typ) <> commaSep(size.map(e => brackets(cExpr(e))))
  }

  def cStmt(cmd : Command) : Doc = cmd match {
    case CmdInitDecl(typ, Id(x), rhs) => {
      cType(typ) <+> value(x) <+> equal <+> cExpr(rhs) <> semi
    }

    case CmdDecl(typ, ids) =>
      cType(typ) <+> commaSep(ids.map({case Id(x) => value(x)}))  <> semi
   
    case CmdPrint(Id(s)) =>
      value("print_1Dtensor") <> parens(value(s))
    
    case CmdRunKernel(Id(f), args) =>
      value("host_" ++ f) <> parens(commaSep(args.map(e => cExpr(e))))
    
    case CmdSeq(c1: Command, c2: Command) => cStmt(c1) <@> cStmt(c2)
    case CmdAssign(Id(x), rhs) => value(x) <+> cExpr(rhs)
    case CmdProcCall(Id(f), args) => cCall(f, args.map(e => cExpr(e))) <> semi
  }

  def cBinop(op : BOp) = op match {
    case NumOp(s) => text(s)
  }

  def cExpr(e : Expr) : Doc = e match {
    case EVar(Id(x)) => value(x)
    case EInt(n) => value(n)
    case EBinop(op, e1, e2) => cExpr(e1) <+> cBinop(op) <+> cExpr(e2)
  }
  
  def decl1dTensor(tensor: Doc, n: Doc): Doc =
    cDeclInit(text("Tensor*"), tensor, cCall("build_1Dtensor", List(quote(tensor), n)))

  def initConst(tensor_name: String, rhs: Doc): Doc =
    cCall("const_init", List(text(tensor_name), rhs)) <> semi

  def print1dTensor(tensor_name: String): Doc =
    cCall("print_1Dtensor", List(text(tensor_name))) <> semi

  def runKernel(kernel_name: String, args: List[Doc]): Doc =
    cCall("host_" ++ kernel_name, args) <> semi

}
