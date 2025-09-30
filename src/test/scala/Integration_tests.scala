package sexprs

import munit.FunSuite
import main.MainFuncs
import parser.Parser
import ast._
import csk.{CSKMachine, RuntimeError}

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
    ),
    (
      """
    ((n = 5.0)
 (one = 1.0)
 (negOne = -1.0)
 (result = 1.0)
 (while0 (n == one)
   (block
     (temp = 0.0)
     (i = 1.0)
     (while0 (i == n)
       (block
         (temp = (temp + result))
         (i = (i + one))))
     (result = temp)
     (n = (n + negOne))))
 result)
    """,
    "1.0"
    ),
    (
      """
    (
  (if0 0.0 
    (block
      (x = -1000.0)
      (y = 1000.0)
      (if0 (x == y)
        (block
          (z = 0.0)
          (w = -0.0))
        (block
          (while0 (x == x)
            (block
              (x = (x + y))
              (if0 (y / x)
                (x = (x + y))
                (y = (x / y))))))))           
    (x = 2.0))
 (x + y))
      """,
      RuntimeError.DivisionByZero("Division by zero error in expression: y / x")
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
          case RuntimeError.DivisionByZero(msg) => assertEquals(RuntimeError.DivisionByZero(msg), expectedOutput)
          case RuntimeError.VarNotFound(msg) => assertEquals(RuntimeError.VarNotFound(msg), expectedOutput)
    }
  }
}