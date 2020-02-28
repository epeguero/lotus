package hbir

import scala.util.parsing.combinator._
import Syntax._




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

  val vvadd_config =
    """
      config {
      |  arrange t(rows, cols) as {
      |    group oneDimGroup: [i in rows*cols] -> [i/cols .. i%cols]
      |  }
      |}
    """.stripMargin

  val vvadd_data =
    """
      data {
       equipartition1D A[n] across G[i] =
         i * A.size[0] / G.size[0] .. (i+1) * A.size[0] / G.size[0]
      }
    """
  val vvadd_code =
    """
     code {
       vvadd (A: int[n], B: int[n]) -> C: int[n]
        | equipartition1D A, B across oneDimGroup[i] =
            A|i, dram|, B|i, dram| ~> A|i, sp|, B|i, sp|:
              let A_sp, B_sp, C_dram = A|i, sp|, B|i, sp|, C|i, dram| in
                for(int i = 0; i < n; i++) {
                  C_dram = A_sp + B_sp
                }
     }
    """

  println("parsing snippet:")
  println(vvadd_config)
  val out = HBIRParser.parse(sample_config)
  println("output:")
  println(out)

  println("testing emission:")
  import Gem5Backend._
  println(Gem5Backend.cBind("x", EInt(5)).pretty)
}
