package hbir

import scala.util.parsing.combinator._
import scala.io.Source
import Syntax._

// largely taken and modified from Dahlia's parser:
// https://github.com/cucapra/dahlia/blob/master/src/main/scala/Parser.scala
private class HBIRParser extends RegexParsers with PackratParsers {
  type P[T] = PackratParser[T]

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


  lazy val codeSection : P[CodeSection] =
    "code" ~> braces(command) ^^ {case c => CodeSection(c)}

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

  def parseCodeSection(str: String): CodeSection = parse(str, codeSection)
  def debug = parse("print(A, B, C);", command)
}


object Main extends App {
  // val code_lines = Source.fromFile("../example/code_section.hbir").getLines
  // val code = code_lines.mkString
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
  println("parsing snippet:")
  // for (line <- code_lines) { println(line) }
  val cs = HBIRParser.parseCodeSection(csStr)
  println("testing emission:")
  println(Gem5Backend.includes.pretty)
  println(Gem5Backend.globalConstants.pretty)
  println(Gem5Backend.main.pretty)
  println(Gem5Backend.code_section(cs).pretty)

}
