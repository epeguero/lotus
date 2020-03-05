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

    val cStatic = text("static")
    val cInt = text("int") 
    val cVoid = text("void")
    val cNot = text("!")
    val cReturn = text("return")
    def cInclude(incl: String): Doc =
      text("#include") <+> text(incl)

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
      case CmdProcCall(Id(f), args) => cFunCall(f, args.map(e => cExpr(e))) <> semi
    }

    def cBinop(op : BOp) = op match {
      case NumOp(s) => text(s)
    }

    def cExpr(e : Expr) : Doc = e match {
      case EVar(Id(x)) => value(x)
      case EInt(n) => value(n)
      case EBinop(op, e1, e2) => cExpr(e1) <+> cBinop(op) <+> cExpr(e2)
    }
  
    def cBinE(e1: Doc, op: Doc, e2: Doc): Doc = 
      e1 <+> op <+> e2

    /**
      * Helper to generate a variable declaration with an initial value.
      */
    def cBind(id: Doc, rhs: Doc): Doc = {
      id <+> "=" <+> rhs <> semi
    }

    def cDeclInit(typ: Doc, id: Doc, rhs: Doc): Doc = 
      typ <+> id <+> "=" <+> rhs <> semi
    
    def cCast(typ: Doc, expr: Doc): Doc =
      parens(typ) <> expr

    /**
      * Helper to generate a function call that might have a type parameter
      */
    def cFunCall(f: Doc, args: List[Doc] = List()): Doc = {
      f <> parens(
        if (args.map(_.h_length).fold(0)(_+_) > 60)
          nest(emptyDoc <@> vsepDelim(",", args), 2)
        else commaSep(args)
      )
    }

    def cFunDef(typ: Doc, f: String, params: List[Doc], body: Doc) = {
      typ <+> text(f) <> parens(commaSep(params)) <+> 
      scope(body)
    }

    def cIf(cond: Doc, true_br: Doc, false_br: Option[Doc] = None) = {
      text("if") <> parens(cond) <@>
      scope(true_br) <@>
      (false_br match{  case None => emptyDoc
                        case Some(stmt) => text("else") <+> scope(stmt) })
    }

    def cFor(inits: Doc, cond: Doc, post: Doc, body: Doc) =
      "for" <> parens(ssep(List(inits, cond, post), semi)) <>
      scope(body)

    def cStructDerefAccess(structName: Doc, fieldName: Doc) =
      structName <> text("->") <> fieldName

    def cArrayAccess(arrayName: Doc, index: Doc) =
      arrayName <> brackets(index)
}

object Gem5Backend {
  import CLike._
  import PrettyPrint.Doc._
  import Syntax._

  val includes : Doc =
    vsep(List(
      "<stdlib.h>", 
      "<stdio.h>",
      "<assert.h>",
      "<string.h>",
      "gem5/header/pthread_launch.h",
      "gem5/header/spad.h",
      "gem5/header/bind_defs.h",
      "lotus_runtime/header/lotus_runtime.h").map((h:String) => cInclude(h)))

  val globalConstants : Doc =
    vsep(
      List(
        ("SP_SIZE", 1000), 
        ("PHYS_ROWS",2), 
        ("PHYS_COLS", 2)).
      map({ case (x, v) => 
          cDeclInit(cStatic <+> cInt, text(x), value(v))} )
    )

  def typedefArgs(k: Kernel) : Doc = k match {
    case Kernel(Id(name), inputs, output, KernelPartition(partStrat, partTensors, Id(groupName), _), body) =>
      val typedefName = name.capitalize ++ "_Args"
      val ins: List[Doc] = inputs.map({case (Id(id),typ) => id})
      val out: Doc = output match {case (Id(id),_) => id}
      "typedef struct" <+> typedefName <+>
      scope(
        vsepDelim(
          ",",
          (ins ++ List(out))
          .map(t => "PartitionedTensor*" <+> t) ++
          List(
            "Group*" <+> groupName,
            "int" <+> "gid")
        )
      ) <+> typedefName <> semi
  }
 
  def buildArgs(k: Kernel) : Doc = k match {
    case Kernel(Id(name), inputs, output, KernelPartition(partStrat, partTensors, Id(groupName), _), body) =>
      val args = "args"
      val argsType = name.capitalize ++ "_Args"
      val structInits : List[Doc] =
        (inputs++List(output)).map(
          (inout: (Id, Type)) => 
            inout match {case (Id(n),_) =>
              "."<>n <+> "=" <+> n }) ++ 
        List( "."<>groupName <+> "=" <+> groupName,
              ".gid" <+> "=" <+> "gid")

      cFunDef(
        argsType <> "**", 
        "build_" ++ name.toLowerCase() + "_args",
        (inputs++List(output)).map({
          case (Id(o), _) => "PartitionedTensor*" <+> o}) ++
        List("Group*" <+> groupName),

        cDeclInit(argsType<>"**", args, 
          cFunCall("malloc", List(
            cBinE(
              cFunCall("sizeof", List(argsType<>"*")),
              "*",
              cArrayAccess(cBinE(groupName,"->","size"), 0)
              )))
        ) <@>

        cFor("int"<+>"gid = 0", "gid < group->size[0]", "gid++",
          cDeclInit(argsType<>"*", "tile_args", 
            cFunCall("malloc", List(
              cFunCall("sizeof", List(argsType))))) <@>
          cBind("*tile_args", cCast(argsType, 
            scope(
              vsepDelim(",", structInits)
            )))  <@>
          cBind("args[gid]", "tile_args")
        ) <@>
        
  cReturn <+> "args" <> semi
      )

  }
  def kernelLauncher(k: Kernel) : Doc = k match {
    case Kernel(Id(name), inputs, output, KernelPartition(partStrat, partTensors, Id(groupName), _), body) =>
      val ins: List[Doc] = inputs.map({case (Id(id),typ) => id})
      val out: Doc = output match {case (Id(id),_) => id}
      val gid: Doc = "gid"
      val tile = "tile"
      val argsType = name.capitalize ++ "_Args"
      val args = "args"
      val a = "a"
      val unpackedInouts= 
        vsep(
          (ins ++ List(out))
            .map(t => 
              cDeclInit("PartitionedTensor*" , t, 
              cBinE(a, "->", t))
            )
        )

      cFunDef(cVoid <> "*", "device_" ++ name, List(cVoid <+> "*"<>args), 
        cDeclInit(argsType <> "*", "a", 
          cCast(argsType<>"*", args)) <@>
        unpackedInouts <@>
        cDeclInit("Group*", groupName, cBinE(a,"->",groupName)) <@>
        cDeclInit("int", gid, cBinE(a,"->",gid)) <@>
          cDeclInit("Tile*", tile, 
            cArrayAccess(groupName<>"->"<>"tiles",gid)) <@>

        pthreadBarrier(
          stats(tile, 
            vectorize(groupName, tile, 
              cFunCall(
                name, 
                ins ++ List(out, groupName, gid)) <> semi
            )
          )
        ) <@>
        cReturn <+> "NULL" <> semi
      )
  }


  def code_section(cs: CodeSection) : Doc = cs match { 
    case CodeSection(Some(cmd)) => cFunDef(text("void"), "code_section", List(), cStmt(cmd))
    case CodeSection(None) => emptyDoc
  }
 
  val main : Doc =
    cFunDef(cVoid, "main", List(), 

      // cIf(cNot <> cFunCall("target_section_is_compatible"),
      //   cFunCall("printf", List(quote(text("Aborting kernel.")))) <> semi <@>
      //   cReturn <> semi
      //   ) <@>
    
      cFunCall("initialize_groups") <> semi <@>
      cFunCall("code_section") <> semi <@>

      cReturn <> semi
    )

  // gem5 constructs
  def vectorize(group: Doc, tile: Doc, body: Doc): Doc =
    cFunCall("VECTOR_EPOCH", List(
      cFunCall("getVecMask",
        List(
          cBinE(tile,"->","phys_tid_col"),
          cBinE(tile,"->","phys_tid_row"),
          cBinE(tile,"->","phys_tid_col"),
          cBinE(tile,"->","phys_tid_row"),
          cBinE(group,"->","num_cols_par"),
          cBinE(group,"->","num_cols_par")
        )))) <@>
    body <@>
    cFunCall("VECTOR_EPOCH", List(0))
     
  def pthreadBarrier(body: Doc) : Doc =
    "pthread_barrier_wait(&start_barrier);" <@>
    body <@>
    "pthread_barrier_wait(&start_barrier);"

  def toggleStats(tile: Doc, toggleOn:Boolean) : Doc =
    cIf(
      cBinE(
        cBinE(
          cBinE(tile, "->", "phys_tid_row"), 
          "==", 0),
        "&&",
        cBinE(
          cBinE(tile, "->", "phys_tid_col"), 
          "==", 0)
      ),
      cFunCall("stats_" ++ (if (toggleOn) "on" else "off")) <> semi
    ) 

  def stats(tile: Doc, body: Doc) =
    toggleStats(tile, true) <@>
    body <@>
    toggleStats(tile, false)


        

  // lotus runtime constructs
  def decl1dTensor(tensor: Doc, n: Doc): Doc =
    cDeclInit(text("Tensor*"), tensor, cFunCall("build_1Dtensor", List(quote(tensor), n)))

  def initConst(tensor_name: String, rhs: Doc): Doc =
    cFunCall("const_init", List(text(tensor_name), rhs)) <> semi

  def print1dTensor(tensor_name: String): Doc =
    cFunCall("print_1Dtensor", List(text(tensor_name))) <> semi

  def runKernel(kernel_name: String, args: List[Doc]): Doc =
    cFunCall("host_" ++ kernel_name, args) <> semi

}
