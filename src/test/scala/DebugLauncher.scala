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
        Point
        (class Point (x y) (method delta (x) (def y (this --> y)) (x = 1.0) (x + y)))
        (((x Number) (y Number)) ((delta (Number) Number))))
        (import Point)
        (def x 1.0)
        (def point (new Point (x x)))
        (point --> x = x)
        (x = (point --> delta (x)))
        x)
      """

    test("Debug launch") {
        val inputSexp = MainFuncs.readSexp(input)
        val result    = runnerFun(inputSexp)
        val outString = result.outputString
        assertEquals(outString, "2.0")
    }
}
