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
        ((tmodule
    C
    (class C
      (f)
      (method
       eq
       (other)
       (def old (this --> f))
       (def myf old)
       (def urf (other --> f))
       (def res 1.0)
       (if0 (old isa C) (this --> f = 42.0) (this --> f = (old + res)))
       (urf = (other --> f))
       (myf = (this --> f))
       (res = (urf == myf))
       (this --> f = old)
       res))
    (((f Number)) ((eq (Number) Number))))
   (import C)
   (def one 1.0)
   (def c (new C (one)))
   (def d c)
   (def e (new C (c)))
   (one = (c --> eq (d)))
   (d = (c --> eq (e)))
   (one + d))
      """

    test("Debug launch") {
        val inputSexp = MainFuncs.readSexp(input)
        val result    = runnerFun(inputSexp)
        val outString = result.outputString
        assertEquals(outString, "\"type error\"")
    }
}
