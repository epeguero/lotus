package hbir

import scala.util.parsing.combinator._
import scala.io.Source
import Syntax._

// largely taken and modified from Dahlia's parser:
// https://github.com/cucapra/dahlia/blob/master/src/main/scala/Parser.scala
private class HBIRParser extends RegexParsers with PackratParsers {
  type P[T] = PackratParser[T]

  // config section
  lazy val configSection: P[ConfigSection] = 
    "config" ~> braces(arrangement.*) ^^ {case as => ConfigSection(as)}

  lazy val arrangement: P[Arrangement] =
    "arrange" ~> iden ~ commaSep(iden) ~ ("as" ~> braces(group.*)) ^^
    {case gridName ~ List(rowId, colId) ~ gs => Arrangement(gridName, (rowId, colId), gs)}

  lazy val group: P[Group] =
    "group" ~> iden ~ (":" ~> coordMap) ^^
    {case name ~ coordMap => Group(name, coordMap)}

  lazy val coordMap : P[CoordMap] =
    (commaSep(brackets(coordIter)) <~ "->") ~ commaSep(brackets(slice)) ^^
    {case domain ~ range => CoordMap(domain, range)}

  lazy val coordIter : P[(Id, Expr)] = 
    ("for" ~> iden <~ "in") ~ expr ^^ {case id ~ e => (id, e) }

  lazy val slice : P[Slice] =
    (expr <~ ":") ~ expr ^^ {case e1 ~ e2 => Slice(e1, e2)}


  // kernel section
  lazy val kernelSection : P[KernelSection] =
    "kernel" ~> braces(kernel.+) ^^ {case ks => KernelSection(ks)}

  lazy val kernel : P[Kernel] =
    iden ~ parens(commaSep(typeAnnotatedVar)) ~ ("->" ~> typeAnnotatedVar) ~
    ("--" ~> kernelPartition <~ "=") ~ kernelStatement ^^
    {case name ~ ins ~ out ~ part ~ stmt => Kernel(name, ins, out, part, stmt)}

  lazy val kernelPartition : P[KernelPartition] = 
    iden ~ commaSep(iden) ~ ("across" ~> iden) ~ brackets(iden).* ^^
      {case strat ~ tensors ~ groupName ~ gid => KernelPartition(strat, tensors, groupName, gid) }

  lazy val kernelStatement : P[KernelStatement] = 
    (commaSep(specifiedTensor) <~ "~>") ~ commaSep(specifiedTensor) ~ (":" ~> kernelStatement) ^^ 
      {case src ~ tgt ~ stmt => KStream(src, tgt, stmt)} |
    (specifiedTensor <~ "=") ~ specifiedTensor ~ ("+" ~> specifiedTensor) <~ ";" ^^ 
      {case out ~ in1 ~ in2 => KVvaddStore(out, in1, in2)}

  lazy val specifiedTensor : P[SpecifiedTensor] =
    iden ~ surround("|", commaSep(iden) ~ ("@" ~> iden))  ^^ 
      {case name ~ (gid ~ memLoc) => SpecifiedTensor(name, gid, memLoc)}

  // code section
  lazy val codeSection : P[CodeSection] =
    "code" ~> braces(command.?) ^^ {case c => CodeSection(c)}

  lazy val command : P[Command] =
    command ~ command ^^ {case c1 ~ c2 => CmdSeq(c1, c2)} |
    typ ~ commaSep(iden) <~ ";" ^^ {case typ ~ ids => CmdDecl(typ, ids) } |
    typ ~ iden ~ ("=" ~> expr) <~ ";" ^^ {case typ ~ id ~ e => CmdInitDecl(typ, id, e)} |
    iden ~ ("=" ~> expr <~ ";") ^^ {case id ~ e => CmdAssign(id, e) } |
    funCall <~ ";" ^^ {case f ~ args => CmdProcCall(f, args)}



  lazy val typ : P[Type] =
    "int" ~> brackets(expr).+ ^^ {case es => TTensor(TInt, es) } |
    "int" ^^ {case _ => TInt}
    
  lazy val expr : P[Expr] =
    int ^^ {n => EInt(n)} |||
    iden ^^ {id => EVar(id)} |||
    expr ~ binop ~ expr ^^ {case e1 ~ op ~ e2 => EBinop(op, e1, e2)}

  lazy val binop : P[BOp] =
    ("*" | "+" | "/" | "%") ^^ { op => NumOp(op) }


  override protected val whiteSpace = """(\s|\/\/.*|(/\*((\*[^/])|[^*])*\*/))+""".r

  // General parser combinators
  def braces[T](parser: P[T]): P[T] = "{" ~> parser <~ "}"
  def brackets[T](parser: P[T]): P[T] = "[" ~> parser <~ "]"
  def parens[T](parser: P[T]): P[T] = "(" ~> parser <~ ")"
  def angular[T](parser: P[T]): P[T] = "<" ~> parser <~ ">"
  def commaSep[T](parser: P[T]): P[List[T]] = repsep(parser, "," )
  def surround[T](surr: String, parser: P[T]) = surr ~> parser <~ surr

  lazy val typeAnnotatedVar : P[(Id, Type)] = (iden <~ ":") ~ typ ^^ {case id ~ typ => (id, typ) }
  lazy val funCall: P[Id ~ List[Expr]] = iden ~ parens(commaSep(expr))

  lazy val iden: P[Id] = positioned {
    "" ~> "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { v => Id(v) }
  }
  lazy val number = "[0-9]+".r ^^ { n => n.toInt } | err("Expected positive number")

  lazy val stringVal: P[String] =
    "\"" ~> "[^\"]*".r <~ "\""

  // Atoms
  lazy val int: P[Int] = "(-)?[0-9]+".r ^^ { n => n.toInt }
  //lazy val hex = "0x[0-9a-fA-F]+".r ^^ { n => Integer.parseInt(n.substring(2), 16) }
  //lazy val octal = "0[0-7]+".r ^^ { n => Integer.parseInt(n.substring(1), 8) }
  //lazy val rational = "(-)?[0-9]+\\.[0-9]+".r ^^ {r => ERational(r)}
  //lazy val boolean = "true" ^^ { _ => true } | "false" ^^ { _ => false }
  
}

object HBIRParser {
  private val parser = new HBIRParser()
  import parser._

  //def parse(str: String): ConfigSection = parseAll(configSection, str) match {
  //  case Success(res, _) => res
  //  case res => throw new Exception("Parse failed.")
  //  //Errors.ParserError(s"$res")
  //}

  def parse[T](str: String, parser: P[T]): T = parseAll(parser, str) match {
    case Success(res, _) => res
    case res => throw new Exception(s"Parse failed:\n${res.toString}")
    //Errors.ParserError(s"$res")
  }

  def parseKernelSection(str: String): KernelSection = parse(str, kernelSection)
  def parseCodeSection(str: String): CodeSection = parse(str, codeSection)
}


object Main extends App {
  // val code_lines = Source.fromFile("../example/code_section.hbir").getLines
  // val code = code_lines.mkString
  val ksStr = """
      |kernel {
      | vvadd (A: int[n], B: int[n]) -> C: int[n]
      | -- equipartition1D A, B across oneDimGroup[i] =
      |      A|i@dram|, B|i@dram| ~> A|i@sp|, B|i@sp|:
      |        C|i@dram| = A|i@sp| + B|i@sp|;
      }
      """.stripMargin

  val csStr= """
      |code {
      |  int n = 100;
      |  int[n] A, B, C;
      |  
      |  A = 1;
      |  B = 2;
      |  C = 0;
      |
      |  print(A); 
      |  print(B); 
      |
      |  vvadd(A, B, C);
      |
      |  print(C);
      |}
      """.stripMargin

  println("testing parsing:")
  val cs = HBIRParser.parseCodeSection(csStr)
  val KernelSection(List(k)) = HBIRParser.parseKernelSection(ksStr)
  println("testing emission:")
  println(Gem5Backend.includes.pretty)
  println("")
  println(Gem5Backend.globalConstants.pretty)
  println("")
  println(Gem5Backend.typedefArgs(k).pretty)
  println("")
  println(Gem5Backend.buildArgs(k).pretty)
  println("")
  println("")
  println(Gem5Backend.hostKernel(k).pretty)
  println("")
  println(Gem5Backend.code_section(cs).pretty)
  println("")
  println(Gem5Backend.main.pretty)

}
