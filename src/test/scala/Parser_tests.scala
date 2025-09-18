package sexprs

import munit.FunSuite
import main.MainFuncs
import ParserAST.Parser
import ast._

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
    val prog      = Parser.parse(inputSexp)
    val has_error = Parser.hasError(prog)
    assertEquals(
      prog,
      Program.Prog(
        stmts = List(
          Statement.Err(
            e = StmtErr.StmtMalformed
          ),
          Statement.Err(
            e = StmtErr.StmtMalformed
          ),
          Statement.Err(
            e = StmtErr.StmtMalformed
          ),
          Statement.Err(
            e = StmtErr.StmtMalformed
          ),
          Statement.Err(
            e = StmtErr.StmtMalformed
          ),
          Statement.Err(
            e = StmtErr.StmtMalformed
          ),
          Statement.Err(
            e = StmtErr.StmtMalformed
          )
        ),
        expr = Expression.Err(
          e = ExprErr.ExprMalformed
        )
      )
    )
    assertEquals(has_error, true)
  }

  val cases = Seq(
    (
      "((foo = 123.4) bar)",
      Program.Prog(
        stmts = List(Statement.Assign(rhs = Expression.Var("foo"), lhs = Expression.Num(123.4))),
        expr = Expression.Var("bar")
      ),
      false
    ),
    (
      """((if0 bar (block (baz = 1.0)) (block (qux = -2.3))) foo)""",
      Program.Prog(
        stmts = List(
          Statement.Ifelse(
            Expression.Var("bar"),
            Block.Many(
              List(
                Statement.Assign(
                  rhs = Expression.Var("baz"),
                  lhs = Expression.Num(1.0)
                )
              )
            ),
            Block.Many(
              List(
                Statement.Assign(
                  rhs = Expression.Var("qux"),
                  lhs = Expression.Num(-2.3)
                )
              )
            )
          )
        ),
        expr = Expression.Var("foo")
      ),
      false
    ),
    (
      """((while0 10.0 (block (foo = 10.0) (bar = -5.5))) (foo == bar))""",
      Program.Prog(
        stmts = List(
          Statement.While(
            guard = Expression.Num(10.0),
            body = Block.Many(
              stmts = List(
                Statement.Assign(
                  Expression.Var("foo"),
                  Expression.Num(10.0)
                ),
                Statement.Assign(
                  Expression.Var("bar"),
                  Expression.Num(-5.5)
                )
              )
            )
          )
        ),
        expr = Expression.Equals(Expression.Var("foo"), Expression.Var("bar"))
      ),
      false
    ),
    (
      """((foo = (bar + baz)) (if0 qux (block (baz = 1.0)) (block (foo = -0.5))) bar)""",
      Program.Prog(
        stmts = List(
          Statement.Assign(
            rhs = Expression.Var("foo"),
            lhs = Expression.Add(
              lhs = Expression.Var("bar"),
              rhs = Expression.Var("baz")
            )
          ),
          Statement.Ifelse(
            guard = Expression.Var("qux"),
            tbranch = Block.Many(
              List(
                Statement.Assign(
                  rhs = Expression.Var("baz"),
                  lhs = Expression.Num(1.0)
                )
              )
            ),
            ebranch = Block.Many(
              List(
                Statement.Assign(
                  rhs = Expression.Var("foo"),
                  lhs = Expression.Num(-0.5)
                )
              )
            )
          )
        ),
        expr = Expression.Var("bar")
      ),
      false
    )
    // Add more cases as needed
  )

  cases.foreach { (input, expected, expectedError) =>
    test(s"Valid Parser Prog + hasError test for input: $input") {
      val inputSexp = MainFuncs.readSexp(input)
      val prog      = Parser.parse(inputSexp)
      val has_error = Parser.hasError(prog)
      assertEquals(prog, expected)
      assertEquals(has_error, expectedError)
    }
  }

}
