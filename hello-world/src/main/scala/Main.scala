import scala.util.parsing.combinator._
import scala.util.parsing.input.{Positional, Position}

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




// largely taken and modified from Dahlia's parser:
// https://github.com/cucapra/dahlia/blob/master/src/main/scala/Parser.scala
private class HBIRParser extends RegexParsers with PackratParsers {
  type P[T] = PackratParser[T]

  lazy val configSection: P[ConfigSection] = 
    "config" ~> braces(arrangement.*) ^^ {case as => ConfigSection(as)}

  lazy val arrangement: P[Arrangement] =
    "arrange" ~> iden ~ parens(repsep(iden,"," )) ~ ("as" ~> braces(group.*)) ^^
    {case gridName ~ List(rowId, colId) ~ gs => Arrangement(gridName, (rowId, colId), gs)}

  lazy val group: P[Group] =
    "group" ~> iden ~ (":" ~> coordMap) ^^
    {case name ~ coordMap => Group(name, coordMap)}

  lazy val coordMap : P[CoordMap] =
    (rep1sep(brackets(coordIter), ",") <~ "->") ~ rep1sep(brackets(slice), ",") ^^
    {case domain ~ range => CoordMap(domain, range)}

  lazy val coordIter : P[(Id, Expr)] =
    (iden <~ "in") ~ expr ^^ {case coord ~ max => (coord, max)} 

  lazy val slice : P[Slice] = 
    (expr <~ "..") ~ expr ^^ {case e1 ~ e2 => Slice(e1, e2)}

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

  def parse(str: String): ConfigSection = parseAll(configSection, str) match {
    case Success(res, _) => res
    case res => throw new Exception("Parse failed.")
    //Errors.ParserError(s"$res")
  }
}

object Main extends App {
  /*
  val config = ConfigSection(
    arrangements=List(
      Arrangement(
        gridName=Id("t"), 
        phys_limit_bindings=(Id("row"), Id("col")), 
        groups=List(
          Group(
            name=Id("oneDimGroup"), 
            mapping=
              CoordMap(
                domain=List(
                  (Id("i"), 
                  EBinop(NumOp("*", OpConstructor.mul), 
                    EVar(Id("row")), 
                    EVar(Id("col"))) )
                ),
                range=List(Slice(
                  EBinop(NumOp("/", OpConstructor.div), 
                    EVar(Id("i")),
                    EVar(Id("col"))),
                  EBinop(NumOp("*", OpConstructor.mod), 
                    EVar(Id("i")),
                    EVar(Id("col")))
                  )
                )
              )
            )
          )
        )
      )
    )

  println(config)
  */

  val sample_config =
    """
      config {
        arrange t(rows, cols) as {
          group oneDimGroup: [i in rows*cols] -> [i/cols .. i%cols]
        }
      }
    """.stripMargin

  println("parsing snippet:")
  println(sample_config)
  val out = HBIRParser.parse(sample_config)
  println("output:")
  println(out)
}
