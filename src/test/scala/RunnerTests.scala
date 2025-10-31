package test

import munit.FunSuite
import main.MainFuncs
import static.Parser
import ast._
import ast.ConverterToClean.progToClean
import main.AssignmentRunner._
import main._

class RunnerTests extends FunSuite {

  val casesA7 = List(
    (
      """
      ((class) 413.0)
      """,
      Result.ParseError,
      "\"parser error\""
    ),
    (
      """
      ((class OWO ()) (class OWO ()) 413.0)
      """,
      Result.DupClassDefs,
      "\"duplicate class name\""
    ),
    (
      """
      ((class OWO ()) (class UWU (a a)) 413.0)
      """,
      Result.DupMethodFieldParams,
      "\"duplicate method, field, or parameter name\""
    ),
    (
      """
      ((class OWO ()) (class UWU (a) (method wow (hai) a)) 413.0)
      """,
      Result.UndefinedVarError,
      "\"undeclared variable error\""
    ),
    (
      """
      ((class OWO ()) 
       (class UWU (a) 
         (method wow (x) (new UWU (x)))
       )
        (def x 413.0) 
        (def o (new UWU (x))) 
        (o --> wow (x)))
      """,
      Result.SuccObj,
      "\"object\""
    ), 
    (
      """
      ((class OWO (a)) 
        (def x 413.0) 
        (def o (new OWO (x))) 
        (o --> a))
      """,
      Result.SuccNum(413.0),
      "413.0"
    )
    , 
    (
      """
      ((class OWO (a)) 
        (def x 413.0) 
        (def o (new OWO (x))) 
        (x --> a = 612.0)
        x)
      """,
      Result.RuntimeError,
      "\"run-time error\""
    )
  )

  val casesA6 = List(
    (
      """
      ((class) 413.0)
      """,
      Result.ParseError,
      "\"parser error\""
    ),
    (
      """
      ((class OWO ()) (class OWO ()) 413.0)
      """,
      Result.DupClassDefs,
      "\"duplicate class name\""
    ),
    (
      """
      ((class OWO ()) (class UWU (a a)) 413.0)
      """,
      Result.DupMethodFieldParams,
      "\"duplicate method, field, or parameter name\""
    ),
    (
      """
      ((class OWO ()) (class UWU (a) (method wow (hai) a)) 413.0)
      """,
      Result.UndefinedVarError,
      "\"undeclared variable error\""
    ),
    (
      """
      ((class OWO ()) (class UWU (a) (method wow (hai) this)) 413.0)
      """,
      Result.ValidityBelongs,
      "\"belongs\""
    )
  )

  val casesA5 = List(
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
      Result.SuccNum(2.0),
      "2.0"
    ),
    (
      """
      ((def x 0.0) (x / x))
      """,
      Result.RuntimeError,
      "\"run-time error\""
    )
  )

  val casesA4 = List(
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

  // val casesA3 = List(
  //   (
  //     """
  //     ((x = 0.0) x y)
  //     """,
  //     Result.ParseError,
  //     "\"parser error\""
  //   ),
  //   (
  //     """
  //     ((x = 0.0) (while0 x (x = 2.0)) x)
  //     """,
  //     Result.SuccNum(2.0),
  //     "2.0"
  //   ),
  //   (
  //     """
  //     ((x = 1.0) (x / y))
  //     """,
  //     Result.RuntimeError,
  //     "\"run-time error\""
  //   )
  // )

  val casesA2 = List(
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

  val casesA1 = List((
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

  val runnerTestsByAssignment = List(
    (7, ceskClass(_), casesA7),
    (6, classParseAndValidity(_), casesA6),
    (5, ceskCore(_), casesA5), 
    (4, coreValidityChecker(_), casesA4),
    // (3, cskBareBones(_), casesA3),
    (2, parserBareBones(_), casesA2),
    (1, startUp(_), casesA1),
  )

  runnerTestsByAssignment.foreach( (assignNum, runnerFun, cases) => 
    cases.foreach ( (input, expectedResult, expectedString) =>
      test(s"Assignment $assignNum Runner test for: $input") {
        val inputSexp = MainFuncs.readSexp(input)
        val result    = runnerFun(inputSexp)
        val outString = result.outputString
        assertEquals(result, expectedResult)
        assertEquals(outString, expectedString)
      }
    )
  )
}
