package sexprs

import munit.FunSuite
import main.MainFuncs
import ParserAST.Parser
import ast._
import csk.CSKMachine
import csk.Control
import csk.RuntimeError

class IntegrationTests extends FunSuite {
  val cases = Seq(
    (
      "((foo = 123.4) bar)",
      RuntimeError.VarNotFound("Variable bar not found in store")
    ),
    (
      "((foo = 123.4) (bar = 0.0) (foo / bar))",
      RuntimeError.DivisionByZero("Division by zero error in expression: foo / bar")
    ),
    (
      "((foo = 123.4) (bar = -2.1) (foo / bar))",
      "-58.76190476190476"
    ),
    (
      "((x = 10.0) (y = 20.0) (x = (x + y)) (x + x))",
      "60.0"
    ),
    (
      "((a = 5.0) (b = 3.0) (a + b))",
      "8.0"
    ),
    (
      "((a = 5.0) (b = 3.0) (a == b))",
      "1.0"
    ),
    (
      "((a = 5.0) (b = 3.0) (a / b))",
      "1.6666666666666667"
    ),
    (
      "(z)",
      RuntimeError.VarNotFound("Variable z not found in store")
    ),
    (
      "((x = 7.0) (y = (x + x)) (y / x))",
      "2.0"
    )
  )

  cases.foreach { case (inputStr, expectedOutput) =>
    test(s"Integration Test: $inputStr") {
      val inputSexp = MainFuncs.readSexp(inputStr)
      val prog      = Parser.parse(inputSexp)

      if Parser.hasError(prog) then
        assertEquals(prog.toString(), expectedOutput)
      else
        CSKMachine.run(prog) match
          case n: Number => assertEquals(n.toString, expectedOutput)
          case Control.Err(e) => e match
            case RuntimeError.DivisionByZero(msg) => assertEquals(RuntimeError.DivisionByZero(msg), expectedOutput)
            case RuntimeError.VarNotFound(msg) => assertEquals(RuntimeError.VarNotFound(msg), expectedOutput)
    }
  }
}