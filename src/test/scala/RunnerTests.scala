package test

import munit.FunSuite
import main.MainFuncs
import frontend.Parser
import ast._
import ast.ConverterToClean.progToClean
import main.AssignmentRunner._
import main._

class RunnerTests extends FunSuite {
  val casesA4 = Seq(
    (
      """
      ((def x 1.0) (def y x) (x = 0.0) x y)
      """,
      Result.ParseError,
      "\"parser error\""
    ),
    (
      """
      ((def y 1.0) (x = 1.0) x)
      """,
      Result.UndefinedVarError,
      "\"undeclared variable error\""
    ),
    (
      """
      ((def x 0.0) (x = 0.0) (while0 x (x = 2.0)) x)
      """,
      Result.ValidityBelongs,
      "\"belongs\""
    )
  )

  casesA4.foreach { (input, expectedResult, expectedString) =>
    test(s"Assignment 4 Runner test for: $input") {
      val inputSexp = MainFuncs.readSexp(input)
      val result    = AssignmentRunner.coreValidityChecker(inputSexp)
      val outString = result.outputString
      assertEquals(result, expectedResult)
      assertEquals(outString, expectedString)
    }
  }

  val casesA2 = Seq(
    (
      """
      ((if0 (c + d) (x = y) a (block (a = b) (while0 8.0 (variable = (liz == l))))) (x / y))
      """,
      Result.ParseError,
      "\"parser error\""
    ),
    (
      """
      ((x = 0.0) (x while0 (if0 (x == x) (x = 1.0) (x = 2.0))) x)
      """,
      Result.ParseError,
      "\"parser error\""
    ),
    (
      """
      ((x = 10.) (y = 10.) (if0 (x == y) (block (x = 5.5)) (block (y = 2.3))) 11.0)
      """,
      Result.ParseBelongs,
      "\"belongs\""
    )
  )
  casesA2.foreach { (input, expectedResult, expectedString) =>
    test(s"Assignment 2 Runner test for: $input") {
      val inputSexp = MainFuncs.readSexp(input)
      val result    = AssignmentRunner.parserBareBones(inputSexp)
      val outString = result.outputString
      assertEquals(result, expectedResult)
      assertEquals(outString, expectedString)
    }
  }

  val casesA1 = Seq((
    """((Someone has created files that contain information of this shape)
        (An Example is one of)
            (a Name like this)
            (a Number like this 22.5 or this -14.3 or this -999.9 or this 999.9)
            (a sequence of space separated Examples wrapped in ( and ))
        () () ()
      )""",
    Result.Count(38),
    "\"38\""
  ))

  casesA1.foreach { (input, expectedResult, expectedString) =>
    test(s"Assignment 1 Runner test for: $input") {
      val inputSexp = MainFuncs.readSexp(input)
      val result    = AssignmentRunner.startUp(inputSexp)
      val outString = result.outputString
      assertEquals(result, expectedResult)
      assertEquals(outString, expectedString)
    }
  }

}
