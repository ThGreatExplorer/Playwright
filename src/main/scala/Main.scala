package main

import sexprs.Lexer
import sexprs.Parser
import sexprs.SExprs._
import java.io.StringReader
import scala.io.StdIn.readLine
import ExampleBB.Checker
import ParserAST.Parser as ParserAST
import csk.CSKMachine
import csk.Control

@main def main(): Unit =
  val inputString = MainFuncs.getMultilineInput()
  val inputSexp = MainFuncs.readSexp(inputString)
  ExampleBB.Checker.assertExampleBB(inputSexp)

  val prog = ParserAST.parse(inputSexp)

  if ParserAST.hasError(prog) then
    println("\"parser error\"")
  else
    // println("\"belongs\"")
    CSKMachine.run(prog) match
      case n: Number => println(n)
      case e: Control.Err => println("\"runtime error\"")

object MainFuncs {

  /** Reads in multi-line input from stdin (w/ readLine) until EOF
    *
    * @return possibly multiline String read-in from stdin
    */
  def getMultilineInput(): String = 
    Iterator.
    continually(readLine).
    takeWhile(_ != null).
    mkString("\n")

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
  * NOTE: For HW1
  *
  * @param input SExpr read from stdio
  * @return integer count of aName elements
  */
  def counter(input :SExpr): Int = 
    input match 
      case SSymbol(x) => 1 
      case SDouble(x) => 0 
      case SList(elements) => elements.map(counter).sum 
      case _ => throw new Exception("SExpr not part of Example Structure: " + input)
}

