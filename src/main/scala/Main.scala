
import sexprs.Lexer
import sexprs.Parser
import sexprs.SExprs._

import java.io.StringReader
import scala.io.StdIn.readLine

@main def main(): Unit =
  val inputString = Iterator.
    continually(readLine).
    takeWhile(_ != null).
    mkString("\n")

  val inputSexp = readSexp(inputString)
  val nameCount = counter(inputSexp)
  println(s"\"$nameCount\"")

/** Parses given input string into an SExpr
  *
  * @param input string, obtained from stdio
  * @return a signle parsed SExpr
  */
def readSexp(input :String): SExpr =
  val reader = new StringReader(input)
  val lexer  = new Lexer(reader)
  val parser = new Parser(lexer)
  parser.parse

/** Counts number of aNames in a given Example s-expression
  * Throws an exception if an unexpected SExpr node is found
  *
  * @param input SExpr read from stdio
  * @return integer count of aName elements
  */
def counter(input :SExpr): Int = 
  input match 
    case SSymbol(x) => 1 //{println(x); 1}
    case SDouble(x) => 0 // {println(x); 0}
    case SList(elements) => elements.map(counter).sum // {println(elements); elements.map(counter).sum} 
    case _ => throw new Exception("SExpr not part of Example Structure: " + input)