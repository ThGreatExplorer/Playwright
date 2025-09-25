/** Challenge Assignment *********************************************************************
 * Q: How would the problem that is mentioned in the dialog between Eli Barzilay and the AI 
 * affect the students in this course? 
 * A: We will inevitably have to deal with floating point numbers in our software development
 * careers, but few people actually know how inexact numbers are handled by our machines.
 * The problem highlighted by Eli Barzilay is incidious because the surface syntax by itself 
 * does not inidicate that either expression is better than other, but there is a tangible 
 * difference. 
 * 
 * Q: How does the design of Bare Bones avoid the problem?
 * A: BareBones does not define a multiplication expression, so floating point rounding errors 
 * introduced by division cannot be amplified. 
 */
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
  def counter(input: SExpr): Int =
    @annotation.tailrec
    def loop(stack: List[SExpr], acc: Int): Int = 
      stack match
        case Nil => acc
        case SSymbol(_) :: rest => loop(rest, acc + 1)
        case SDouble(_) :: rest => loop(rest, acc)
        case SList(elements) :: rest => loop(elements ++ rest, acc)
        case head :: _ => throw new Exception("SExpr not part of Example Structure: " + head)
    loop(List(input), 0)
}

