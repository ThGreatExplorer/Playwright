package test

import munit.FunSuite
import main.MainFuncs
import static.Parser
import ast._
import ast.ConverterToClean.progToClean
import main.AssignmentRunner._
import main._

class RunnerTests extends FunSuite {

  val casesA11 = List(
    (
      """
      (
        (tmodule Body (class A () (method f () 413.0)) (() ((f () Number))))
        (import Body)
        (def objA (new A()))
        (objA --> f())
      )
      """,
      Result.ParseError,
      "\"parser error\""
    ),
    (
      """
      (
        (tmodule A (class A () (method f () 413.0)) (() ((f () Number))))
        (module A (class A () (method g () 413.0)))
        (import A)
        (def objA (new A()))
        (objA --> f())
      )
      """,
      Result.DupModuleDefs,
      "\"duplicate module name\""
    ),
    (
      """
      (
      (tmodule A (class B (a b) (method f () 413.0)) (() ((f () Number) (f () Number))))
      (import B)
      (def objB (new B()))
      (objB --> f())
      )
      """,
      Result.DupMethodFieldParams,
      "\"duplicate method, field, or parameter name\""
    ),
    (
      """
      (
        (module A (class A () (method f () 413.0)))
        (tmodule B (timport A (() ((f () Number)))) (timport A (() ((g () Number))))
          (class B () (method g () 413.0)) 
          (() ((g () Number))))
        (import B)
        (def objB (new B()))
        objB
      )
      """,
      Result.BadImport,
      "\"bad import\""
    ),
    (
      """
      (
      (tmodule A (class B () (method f () 413.0)) (() ((f () Number))))
      (import B)
      (def objB (new B()))
      (objB --> f())
      )
      """,
      Result.UndefinedVarError,
      "\"undeclared variable error\""
    ),
    (
      """
      ((module A (class c ()))
      (tmodule B (class c () (method m () 2.0)) (() ((m () Number))))
      (timport A (() ()))
      (import B)
      (timport A (() ()))
      (import B)
      (timport A (() ()))
      (import B)
      (timport A (() ()))
      (def ex (new c ()))
      (def res (ex --> m ()))
      res)
      """,
      Result.TypeError,
      "\"type error\"" 
    ), 
    ( """
      (
        (tmodule Helper
          (class Helper () (method faveNum () 413.0))
          (() ((faveNum () Number)))
        )
        (module Insidious (import Helper)
          (class Insidious () 
            (method violation () 
              (def o (new Helper ()))
              (def val (o --> faveNum()))
              (val isa Helper)
            )
          )
        )
        (timport Insidious (() ((violation () Number))))
        (def o (new Insidious ()))
        (o --> violation ())
      )
      """,
      Result.SynthesizedModuleNames(List("Helper", "Insidious", "Insidious.into.Body")),
      "[\"Helper\",\"Insidious\",\"Insidious.into.Body\"]"
    ), 
    ( """
      (
        (module Cowboy (class Cowboy() (method draw() 1.0)))
        (module Artist (class Artist() (method draw() 666.0)))
        (module MiniMain 
          (import Cowboy)
          (import Artist)
          (class MiniMain()
            (method main() 
              (def a (new Artist ()))
              (def c (new Cowboy ()))
              (def x 0.0)
              (if0 1.0 (x = a) (x = c))
              (x = (x --> draw ()))
              x)
          )
        )
        (timport MiniMain (()((main () Number))))
        (def obj (new MiniMain()))
        (obj --> main())
      )
      """,
      Result.SynthesizedModuleNames(List("Cowboy", "Artist", "MiniMain", "MiniMain.into.Body")),
      "[\"Cowboy\",\"Artist\",\"MiniMain\",\"MiniMain.into.Body\"]"
    )
  )

  val casesA10 = List(
    (
      """
      (
        (module A (class A () (method f () 413.0)))
        (module B (timport A (() ((f () Number)))) (class B () (method g () 413.0)))
        (import B)
        (def objB (new B()))
        objB
      )
      """,
      Result.ParseError,
      "\"parser error\""
    ),
    (
      """
      (
        (tmodule A (class A () (method f () 413.0)) (() ((f () Number))))
        (module A (class A () (method g () 413.0)))
        (import A)
        (def objA (new A()))
        (objA --> f())
      )
      """,
      Result.DupModuleDefs,
      "\"duplicate module name\""
    ),
    (
      """
      (
      (tmodule A (class B (a b) (method f () 413.0)) (() ((f () Number) (f () Number))))
      (import B)
      (def objB (new B()))
      (objB --> f())
      )
      """,
      Result.DupMethodFieldParams,
      "\"duplicate method, field, or parameter name\""
    ),
    (
      """
      (
        (module A (class A () (method f () 413.0)))
        (tmodule B (timport A (() ((f () Number)))) (timport A (() ((g () Number))))
          (class B () (method g () 413.0)) 
          (() ((g () Number))))
        (import B)
        (def objB (new B()))
        objB
      )
      """,
      Result.BadImport,
      "\"bad import\""
    ),
    (
      """
      (
      (tmodule A (class B () (method f () 413.0)) (() ((f () Number))))
      (import B)
      (def objB (new B()))
      (objB --> f())
      )
      """,
      Result.UndefinedVarError,
      "\"undeclared variable error\""
    ),
    (
      """
      (
        (module A (class A () (method f () 413.0)))
        (tmodule B (timport A (() ((f () Number))))
          (class B () 
            (method g () 
              (def objA (new A())) 
              (def val (objA --> f()))
              (val isa A))) 
          (() ((g () Number))))
        (import B)
        (def objB (new B()))
        objB
      )
      """,
      Result.TypeError,
      "\"type error\""
    ), 
    ( // Special Incidious example 
      """
      (
        (tmodule Helper
          (class Helper () (method faveNum () 413.0))
          (() ((faveNum () Number)))
        )
        (module Insidious (import Helper)
          (class Insidious () 
            (method violation () 
              (def o (new Helper ()))
              (def val (o --> faveNum()))
              (val isa Helper)
            )
          )
        )
        (timport Insidious (() ((violation () Number))))
        (def o (new Insidious ()))
        (o --> violation ())
      )
      """,
      Result.SuccNum(1.0),
      "1.0"
    ), 
    ( // Special Blatant example 
      """
      (
        (module Blatant 
          (class Blatant () 
            (method violation () 
              this
            )
          )
        )
        (timport Blatant (() ((violation () Number))))
        (def bla (new Blatant ()))
        (bla --> violation ())
      )
      """,
      Result.SuccObj,
      "\"object\""
    ),
    (
      """
      (
      (tmodule A (class A () (method f () 413.0)) (() ((f () Number))))
      (import A)
      (def objA (new A()))
      (def num (objA --> f()))
      (def zero 0.0)
      (num / zero)
      )
      """,
      Result.RuntimeError,
      "\"run-time error\""
    )
  )

  val casesA9 = List(
    (
      """
      (
        (module A (class A () (method f () 413.0)))
        (import A)
        (def objA (new A()))
        (objA --> f())
      )
      """,
      Result.ParseError,
      "\"parser error\""
    ),
    (
      """
      (
        (tmodule A (class A () (method f () 413.0)) (() ((f () Number))))
        (tmodule A (class A () (method g () 413.0)) (() ((g () Number))))
        (import A)
        (def objA (new A()))
        (objA --> f())
      )
      """,
      Result.DupModuleDefs,
      "\"duplicate module name\""
    ),
    (
      """
      (
        (tmodule A (class B (a b) (method f () 413.0)) (() ((f () Number) (f () Number))))
        (import B)
        (def objB (new B()))
        (objB --> f())
      )
      """,
      Result.DupMethodFieldParams,
      "\"duplicate method, field, or parameter name\""
    ),
    (
      """
      (
        (tmodule A (class B () (method f () 413.0)) (() ((f () Number))))
        (import B)
        (def objB (new B()))
        (objB --> f())
      )
      """,
      Result.UndefinedVarError,
      "\"undeclared variable error\""
    ),
    (
      """
      (
        (tmodule A (class A () (method f () 413.0)) (() ((f () Number))))
        (tmodule B (class B () (method g () 413.0)) (() ((g () Number))))
        (import A)
        (import B)
        (def objA (new A()))
        (def objB (new B()))
        (objA = objB)
        objA
      )
      """,
      Result.TypeError,
      "\"type error\""
    ), 
    (
      """
      (
        (tmodule A (class A () (method f () 413.0)) (() ((f () Number))))
        (import A)
        (def objA (new A()))
        (objA --> f())
      )
      """,
      Result.SuccNum(413.0),
      "413.0"
    ), 
    (
      """
      (
        (tmodule A (class A () (method f () 413.0)) (() ((f () Number))))
        (import A)
        (def objA (new A()))
        (def num (objA --> f()))
        (def zero 0.0)
        (num / zero)
      )
      """,
      Result.RuntimeError,
      "\"run-time error\""
    )
  )

  val casesA8 = List(
    (
      """
      ((module) 413.0)
      """,
      Result.ParseError,
      "\"parser error\""
    ),
    (
      """
      ((module UWU (class OWO ())) (module UWU (class OWO ())) 413.0)
      """,
      Result.DupModuleDefs,
      "\"duplicate module name\""
    ),
    (
      """
      ((module UWU (class OWO (a a))) 413.0)
      """,
      Result.DupMethodFieldParams,
      "\"duplicate method, field, or parameter name\""
    ),
    (
      """
      ((module UWU (class OWO (a) (method wow (hai) a))) 413.0)
      """,
      Result.UndefinedVarError,
      "\"undeclared variable error\""
    ),
    (
      """
      ((module OWO 
       (class UWU (a) 
         (method wow (x) (new UWU (x)))
       ))
       (import OWO)
        (def x 413.0) 
        (def o (new UWU (x))) 
        (o --> wow (x)))
      """,
      Result.SuccObj,
      "\"object\""
    ), 
    (
      """
      ((module OWO 
       (class UWU (a)))
       (import OWO)
        (def x 413.0) 
        (def o (new UWU (x))) 
        (o --> a))
      """,
      Result.SuccNum(413.0),
      "413.0"
    ), 
    (
      """
      ((module OWO 
       (class UWU (a)))
       (import OWO)
        (def x 413.0) 
        (def o (new UWU (x))) 
        (x --> a = 612.0)
        x)
      """,
      Result.RuntimeError,
      "\"run-time error\""
    )
  )

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
    (11, mixedLinking(_), casesA11),
    (10, mixedSystem(_), casesA10),
    (9, typedSystem(_), casesA9),
    (8, ceskModule(_), casesA8),
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
