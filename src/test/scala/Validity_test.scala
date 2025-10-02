package sexprs

import munit.FunSuite
import main.MainFuncs
import frontend.Parser
import ast._
import ast.ConverterToClean.progToClean
import ParserTests.validCases
import frontend.ValidityChecker

class ValidityTest extends FunSuite {

  val expectedValues: Seq[(CleanProgram, Boolean)] = Seq.fill(validCases.size)(
    // dummy cleaned program since all of the defined examples fail
    ((CleanProgram(List(), List(), CleanExpr.Num(1.0))), true)
  )

  validCases.zip(expectedValues).foreach{ case (input, (expected, expectedError)) => 
    test(s"Well-formed Parser Prog Invalid test for input: $input") {
      val inputSexp = MainFuncs.readSexp(input._1)
      val prog      = Parser.parseProg(inputSexp)
      progToClean(prog) match
        case None => throw new Exception("Not Well Formed Parser Prog")
        case Some(cleanProg) => 
          val validatedProg = ValidityChecker.closedProg(cleanProg)
        
          progToClean(validatedProg) match 
            case None => assertEquals(true, expectedError)
            case Some(validProg) => 
              assertEquals(false, expectedError) 
              assertEquals(validProg, expected)
    }
  }

  ValidityTests.validCases.foreach{ (input, expected, expectedError) => 
    test(s"Well-formed Parser Prog Valid test for input: $input") {
      val inputSexp = MainFuncs.readSexp(input)
      val prog      = Parser.parseProg(inputSexp)
      progToClean(prog) match
        case None => throw new Exception("Not Well Formed Parser Prog")
        case Some(cleanProg) => 
          val validatedProg = ValidityChecker.closedProg(cleanProg)
        
          progToClean(validatedProg) match 
            case None => assertEquals(true, expectedError)
            case Some(validProg) => 
              assertEquals(false, expectedError) 
              assertEquals(validProg, expected)
    }
  }
}

object ValidityTests {
  val validCases = Seq(
  (
    "((def foo 123.4) (foo = 100.0) foo)",
    CleanProgram(
      decls = List(
        CleanDecl(CleanExpr.Var("foo"), CleanExpr.Num(123.4))
      ),
      stmts = List(
        CleanStmt.Assign(CleanExpr.Var("foo"), CleanExpr.Num(100.0))
      ),
      expr = CleanExpr.Var("foo")
    ),
    false
  ),
  (
    "((def a 1.0) (def b 2.0) (a = (a + b)) (a / b))",
    CleanProgram(
      decls = List(
        CleanDecl(CleanExpr.Var("a"), CleanExpr.Num(1.0)),
        CleanDecl(CleanExpr.Var("b"), CleanExpr.Num(2.0))
      ),
      stmts = List(
        CleanStmt.Assign(
          lhs = CleanExpr.Var("a"),
          rhs = CleanExpr.BinOpExpr(CleanExpr.Var("a"), BinOp.Add, CleanExpr.Var("b"))
        )
      ),
      expr = CleanExpr.BinOpExpr(CleanExpr.Var("a"), BinOp.Div, CleanExpr.Var("b"))
    ),
    false
  ),
  (
    "((def x 10.0) (if0 x (block (def y 2.0) (y = 5.0)) (block (def z 3.0) (z = 7.0))) x)",
    CleanProgram(
      decls = List(
        CleanDecl(CleanExpr.Var("x"), CleanExpr.Num(10.0))
      ),
      stmts = List(
        CleanStmt.Ifelse(
          guard = CleanExpr.Var("x"),
          tbranch = CleanBlock.Many(
            decls = List(
              CleanDecl(CleanExpr.Var("y"), CleanExpr.Num(2.0))
            ),
            stmts = List(
              CleanStmt.Assign(CleanExpr.Var("y"), CleanExpr.Num(5.0))
            )
          ),
          ebranch = CleanBlock.Many(
            decls = List(
              CleanDecl(CleanExpr.Var("z"), CleanExpr.Num(3.0))
            ),
            stmts = List(
              CleanStmt.Assign(CleanExpr.Var("z"), CleanExpr.Num(7.0))
            )
          )
        )
      ),
      expr = CleanExpr.Var("x")
    ),
    false
  ),
  (
    "((def guard 1.0) (while0 guard (block (def i 0.0) (i = (i + guard)))) guard)",
    CleanProgram(
      decls = List(
        CleanDecl(CleanExpr.Var("guard"), CleanExpr.Num(1.0))
      ),
      stmts = List(
        CleanStmt.While(
          guard = CleanExpr.Var("guard"),
          body = CleanBlock.Many(
            decls = List(
              CleanDecl(CleanExpr.Var("i"), CleanExpr.Num(0.0))
            ),
            stmts = List(
              CleanStmt.Assign(
                CleanExpr.Var("i"),
                CleanExpr.BinOpExpr(CleanExpr.Var("i"), BinOp.Add, CleanExpr.Var("guard"))
              )
            )
          )
        )
      ),
      expr = CleanExpr.Var("guard")
    ),
    false
  ),
  (
    "((def a 10.0) (def b 20.0) (a = (a + b)) (if0 a (block (def c 1.0) (c = (a / b))) (block (def d 2.0) (d = (b / a)))) (a == b))",
    CleanProgram(
      decls = List(
        CleanDecl(CleanExpr.Var("a"), CleanExpr.Num(10.0)),
        CleanDecl(CleanExpr.Var("b"), CleanExpr.Num(20.0))
      ),
      stmts = List(
        CleanStmt.Assign(
          CleanExpr.Var("a"),
          CleanExpr.BinOpExpr(CleanExpr.Var("a"), BinOp.Add, CleanExpr.Var("b"))
        ),
        CleanStmt.Ifelse(
          guard = CleanExpr.Var("a"),
          tbranch = CleanBlock.Many(
            decls = List(
              CleanDecl(CleanExpr.Var("c"), CleanExpr.Num(1.0))
            ),
            stmts = List(
              CleanStmt.Assign(
                CleanExpr.Var("c"),
                CleanExpr.BinOpExpr(CleanExpr.Var("a"), BinOp.Div, CleanExpr.Var("b"))
              )
            )
          ),
          ebranch = CleanBlock.Many(
            decls = List(
              CleanDecl(CleanExpr.Var("d"), CleanExpr.Num(2.0))
            ),
            stmts = List(
              CleanStmt.Assign(
                CleanExpr.Var("d"),
                CleanExpr.BinOpExpr(CleanExpr.Var("b"), BinOp.Div, CleanExpr.Var("a"))
              )
            )
          )
        )
      ),
      expr = CleanExpr.BinOpExpr(CleanExpr.Var("a"), BinOp.Equals, CleanExpr.Var("b"))
    ),
    false
  )
)
}