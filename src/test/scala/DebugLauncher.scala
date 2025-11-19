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
    
    val runnerFun = mixedSystem(_)
    val input = 
        """
    (
      (module A (class A () ))
      (tmodule B (timport A (() ())) (class B ()) (() ()))
      (timport A (((x Number)) ()))
      (import B)
      1.0
    )
      """

    test("Debug launch") {
        val inputSexp = MainFuncs.readSexp(input)
        val result    = runnerFun(inputSexp)
        val outString = result.outputString
        assertEquals(outString, "1.0")
    }
}