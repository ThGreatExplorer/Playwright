package main

import java.io.StringReader
import scala.io.StdIn.readLine

import sexprs.Lexer
import sexprs.Parser
import sexprs.SExprs.SExpr
import util.ExampleChecker.assertExampleCC

@main def main(): Unit =
  val inputString = MainFuncs.getMultilineInput()
  val inputSexp = MainFuncs.readSexp(inputString)
  assertExampleCC(inputSexp)

  val result = AssignmentRunner.cskBareBones(inputSexp)
  println(result.outputString)

object MainFuncs:
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


