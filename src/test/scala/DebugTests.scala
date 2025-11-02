package test

import munit.FunSuite
import main.MainFuncs
import static.Parser
import ast._
import main.AssignmentRunner.ceskModule
import main.Result

class DebugTests extends FunSuite {
  
  val testCaseToDebug = """
  ( 
  (module OWO (class AClass (fave))) 
  (module UWU (import OWO)
    (class AClass () 
      (method makeWithFave (fave) 
        (new AClass (fave)))
      (method updateFave (o fave)
        (o --> fave = fave)
        0.0)))

  (import UWU)
  (import UWU)

  (def fave 413.0)
  (def anotherFave 612.0)
  (def oOne (new AClass ()))
  (def oTwo (oOne --> makeWithFave(fave)))
  (def status (oOne --> updateFave(oTwo anotherFave)))

  (oTwo --> fave)
)
  """

  test("Debugging test case") {
    val runnerFun = ceskModule(_)
    val inputSexp = MainFuncs.readSexp(testCaseToDebug)
    val result    = runnerFun(inputSexp)
    val outString = result.outputString
    assertEquals(result, Result.RuntimeError)
  }
}