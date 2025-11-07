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
    
    val runnerFun = typedSystem(_)
    val input = 
        """
      (
    (module ModuleAOne (class A () (method onlyAOne () 0.0)) (() ((onlyAOne () Number))))
    (module ModuleATwo (class A ()) (() ()))

    (module ModuleBAOne (import ModuleAOne) (import ModuleATwo) 
        (class B () (method newA () (new A ())))
        (() ((newA () (() ())))))

    (import ModuleAOne)
    (import ModuleBAOne)

    (def b (new B ()))
    (def someA (b --> newA ()))

    (someA --> onlyAOne())
) 

      """

    test("Debug launch") {
        val inputSexp = MainFuncs.readSexp(input)
        val result    = runnerFun(inputSexp)
        val outString = result.outputString
        assertEquals(outString, "2.0")
    }
}
