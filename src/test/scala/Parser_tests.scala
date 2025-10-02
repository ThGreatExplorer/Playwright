package test

import munit.FunSuite
import main.MainFuncs
import frontend.Parser
import ast._
import ast.ConverterToClean.progToClean

class ParserTests extends FunSuite {
  test("Test Invalid Example Parser + hasError") {
    // Every single statement and expression is invalid
    val test_str1 = """((Someone has created files that contain information of this shape)
        (An Example is one of)
            (a Name like this)
            (a Number like this 22.5 or this -14.3 or this -999.9 or this 999.9)
            (a sequence of space separated Examples wrapped in ( and ))
        () () ()
    )"""
    val inputSexp = MainFuncs.readSexp(test_str1)
    val prog      = Parser.parseProg(inputSexp)
    val hasError = progToClean(prog).isEmpty
    assertEquals(
      prog,
      ProgramWE.Prog(
        decls = List(),
        stmts = List(
          StmtWE.Err(
            e = StmtErr.Malformed
          ),
          StmtWE.Err(
            e = StmtErr.Malformed
          ),
          StmtWE.Err(
            e = StmtErr.Malformed
          ),
          StmtWE.Err(
            e = StmtErr.Malformed
          ),
          StmtWE.Err(
            e = StmtErr.Malformed
          ),
          StmtWE.Err(
            e = StmtErr.Malformed
          ),
          StmtWE.Err(
            e = StmtErr.Malformed
          )
        ),
        expr = ExprWE.Err(
          e = ExprErr.Malformed
        )
      )
    )
    assertEquals(hasError, true)
  }

  val cases = Seq(
    (
      "((foo = 123.4) bar)",
      ProgramWE.Prog(
        decls = List(),
        stmts = List(StmtWE.Assign(lhs = ExprWE.Var("foo"), rhs = ExprWE.Num(123.4))),
        expr = ExprWE.Var("bar")
      ),
      false
    ),
    (
      """((if0 bar (block (baz = 1.0)) (block (qux = -2.3))) foo)""",
      ProgramWE.Prog(
        decls = List(),
        stmts = List(
          StmtWE.Ifelse(
            ExprWE.Var("bar"),
            BlockWE.Many(
              List(),
              List(
                StmtWE.Assign(
                  lhs = ExprWE.Var("baz"),
                  rhs = ExprWE.Num(1.0)
                )
              )
            ),
            BlockWE.Many(
              List(),
              List(
                StmtWE.Assign(
                  ExprWE.Var("qux"),
                  ExprWE.Num(-2.3)
                )
              )
            )
          )
        ),
        expr = ExprWE.Var("foo")
      ),
      false
    ),
    (
      """((while0 10.0 (block (foo = 10.0) (bar = -5.5))) (foo == bar))""",
      ProgramWE.Prog(
        decls = List(),
        stmts = List(
          StmtWE.While(
            guard = ExprWE.Num(10.0),
            body = BlockWE.Many(
                List(),
              stmts = List(
                StmtWE.Assign(
                  ExprWE.Var("foo"),
                  ExprWE.Num(10.0)
                ),
                StmtWE.Assign(
                  ExprWE.Var("bar"),
                  ExprWE.Num(-5.5)
                )
              )
            )
          )
        ),
        expr = ExprWE.BinOpExpr(ExprWE.Var("foo"), BinOp.Equals, ExprWE.Var("bar"))
      ),
      false
    ),
    (
      """((foo = (bar + baz)) (if0 qux (block (baz = 1.0)) (block (foo = -0.5))) bar)""",
      ProgramWE.Prog(
        decls = List(),
        stmts = List(
          StmtWE.Assign(
            lhs = ExprWE.Var("foo"),
            rhs = ExprWE.BinOpExpr(
              lhs = ExprWE.Var("bar"),
              BinOp.Add,
              rhs = ExprWE.Var("baz")
            )
          ),
          StmtWE.Ifelse(
            guard = ExprWE.Var("qux"),
            tbranch = BlockWE.Many(
                List(),
              List(
                StmtWE.Assign(
                  lhs = ExprWE.Var("baz"),
                  rhs = ExprWE.Num(1.0)
                )
              )
            ),
            ebranch = BlockWE.Many(
                List(),
              List(
                StmtWE.Assign(
                  lhs = ExprWE.Var("foo"),
                  rhs = ExprWE.Num(-0.5)
                )
              )
            )
          )
        ),
        expr = ExprWE.Var("bar")
      ),
      false
    )
    // Add more cases as needed
  )

  cases.foreach { (input, expected, expectedError) =>
    test(s"Valid Parser Prog + hasError test for input: $input") {
      val inputSexp = MainFuncs.readSexp(input)
      val prog      = Parser.parseProg(inputSexp)
      val has_error = progToClean(prog).isEmpty
      assertEquals(prog, expected)
      assertEquals(has_error, expectedError)
    }
  }

}
