package test

import munit.FunSuite
import main.MainFuncs
import util.ExampleChecker.assertExample
import util._

class ExampleRunnerTest extends FunSuite:

    test("ExampleChecker catches numbers in names") {
        val testStr = "(An Example can have Name like this but not like this ilike413)"
        val inputSexp = MainFuncs.readSexp(testStr)
        interceptMessage[InputNotExampleException]("SExpr contains SSymbol that is not an ExampleBB Name: ilike413") {
            assertExample(inputSexp)
        }
    }

    test("ExampleChecker catches long names") {
        val testStr = "(An Example can have Name like this but not like this owowwoowoowowowowowow)"
        val inputSexp = MainFuncs.readSexp(testStr)
        interceptMessage[InputNotExampleException]("SExpr contains SSymbol that is not an ExampleBB Name: owowwoowoowowowowowow") {
            assertExample(inputSexp)
        }
    }

    test("ExampleChecker catches bad positive numbers") {
        val testStr = "(An Example can have Numbers like this -.9 or this .9 but not 1000.2)"
        val inputSexp = MainFuncs.readSexp(testStr)
        interceptMessage[InputNotExampleException]("SExpr contains SDouble that is not an ExampleBB Number: 1000.2") {
            assertExample(inputSexp)
        }
    }

    test("ExampleChecker catches bad negative numbers") {
        val testStr = "(An Example can have Numbers like this -.9 or this .9 but not -1040.4)"
        val inputSexp = MainFuncs.readSexp(testStr)
        interceptMessage[InputNotExampleException]("SExpr contains SDouble that is not an ExampleBB Number: -1040.4") {
            assertExample(inputSexp)
        }
    }

    test("ExampleChecker catches non doubles") {
        val testStr = "(An Example cannot have (((integers)) like (this)) 413)"
        val inputSexp = MainFuncs.readSexp(testStr)
        interceptMessage[InputNotExampleException]("SExpr not part of Example Structure: SInt(413)") {
            assertExample(inputSexp)
        }
    }


