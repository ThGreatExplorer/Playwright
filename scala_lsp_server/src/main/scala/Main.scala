package main

import java.io.StringReader
import scala.io.StdIn.readLine

import sexprs.Lexer
import sexprs.Parser
import sexprs.SExprs.SExpr
import util.ExampleChecker.assertExample
import org.eclipse.lsp4j.launch.LSPLauncher

enum RunMode:
  case Backend, Server

object Main:
  def main(args: Array[String]): Unit =
    run(RunMode.Backend, args)

  def run(mode: RunMode, args: Array[String]): Unit =
    mode match
      case RunMode.Backend => BackendRunner.run()
      case RunMode.Server => ServerRunner.run(args)

object BackendRunner:
  def main(args: Array[String]): Unit =
    run()

  def run(): Unit =
    val inputString = BackendFuncs.getMultilineInput()
    val inputSexp = BackendFuncs.readSexp(inputString)
    assertExample(inputSexp)

    val result = AssignmentRunner.mixedSound(inputSexp)
    println(result.outputString)

object ServerRunner:
  def main(args: Array[String]): Unit =
    run(args)

  def run(args: Array[String]): Unit =
    val serverInstance = new server.TestLanguageServer()
    val launcher =
      LSPLauncher.createServerLauncher(serverInstance, System.in, System.out)
    val future = launcher.startListening()
    future.get()

object BackendFuncs:
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
