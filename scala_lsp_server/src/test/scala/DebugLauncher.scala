package test

package test

import munit.FunSuite
import main.MainFuncs
import static.Parser
import ast._
import ast.ConverterToClean.progToClean
import main.AssignmentRunner._
import main._

class RunnerTests extends FunSuite {
    
    val runnerFun = mixedSound(_)
    val input = 
        """
      (
        (tmodule A (class A () (method f () 413.0)) (() ((f () Number))))
        (import A)
        (def objA (new A()))
        (objA --> f())
      )
      """

    test("Debug launch") {
        val inputSexp = MainFuncs.readSexp(input)
        val result    = runnerFun(inputSexp)
        val outString = result.outputString
        assertEquals(outString, "413.0")
    }
}