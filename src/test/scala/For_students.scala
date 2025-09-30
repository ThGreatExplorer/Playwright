package sexprs

import munit.FunSuite
import main.MainFuncs
import parser.Parser
import ast._
import csk.{CSKMachine, Control, RuntimeError}
import ast.ASTInspector.progHasError

class ForStudents extends FunSuite {
  val cases = Seq(
    (
      "((x = 0.0) x y)", // input string
      "parser error"
    ),
    (
      "((x = 1.0) x)",
      "1.0"
    ), 
    (
      "((x = 0.0) (while0 x (x = 2.0)) x)",
      "2.0"
    )
  )

  cases.foreach { case (inputStr, expectedOutput) =>
    test(s"For Students 3 Test: $inputStr") {
      val inputSexp = MainFuncs.readSexp(inputStr)
      val prog      = Parser.parseProg(inputSexp)

      if progHasError(prog) then
        assertEquals("parser error", expectedOutput)
      else
        CSKMachine.run(prog) match
          case n: Number => assertEquals(n.toString, expectedOutput)
          case e: RuntimeError => assertEquals(e.toString, expectedOutput)
    }
  }
}