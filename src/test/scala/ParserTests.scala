package test

import munit.FunSuite
import main.MainFuncs
import frontend.Parser
import ast._
import ast.ConverterToClean.progToClean

class ParserTests extends FunSuite {
  
  ParserTests.invalidCases.foreach { (input, expected, expectedError) =>
    test(s"Ill-formed Parser Prog + hasError test for input: $input") {
      val inputSexp = MainFuncs.readSexp(input)
      val prog      = Parser.parseProg(inputSexp)
      val hasError = progToClean(prog).isEmpty
      assertEquals(prog, expected)
      assertEquals(hasError, expectedError)
    }
  }

  ParserTests.validCases.foreach { (input, expected, expectedError) =>
    test(s"Well-formed Parser Prog + hasError test for input: $input") {
      val inputSexp = MainFuncs.readSexp(input)
      val prog      = Parser.parseProg(inputSexp)
      val hasError = progToClean(prog).isEmpty
      assertEquals(prog, expected)
      assertEquals(hasError, expectedError)
    }
  }

}

object ParserTests:
  val validCases = Seq(
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


  val invalidCases = Seq(
    (
       """((Someone has created files that contain information of this shape)
        (An Example is one of)
            (a Name like this)
            (a Number like this 22.5 or this -14.3 or this -999.9 or this 999.9)
            (a sequence of space separated Examples wrapped in ( and ))
        () () ()
    )""",
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
      ),
      true
    ),
    (
      """
      ((foo = ) =)
      """,
      ProgramWE.Prog(
        decls = List(),
        stmts = List(
          StmtWE.Err(StmtErr.AssignRhsMalformed)
        ),
        expr = 
          ExprWE.VarErrNode(
            e = VarErr.IsKeyword
          )
      ),
      true
    ),
    (
      "()",
      ProgramWE.Err(ProgErr.EmptyList),
      true
    ),
    (
      "((def x 1.0) (def y 2.0) (def x) (x + y))",
      ProgramWE.Prog(
        decls = List(
          DeclWE.Def(
            ExprWE.Var("x"),
            ExprWE.Num(1.0),
          ),
          DeclWE.Def(
            ExprWE.Var("y"),
            ExprWE.Num(2.0)
          ),
          DeclWE.Err(DeclErr.Malformed),
        ),
        stmts = List(),
        ExprWE.BinOpExpr(ExprWE.Var("x"),BinOp.Add,ExprWE.Var("y"))
      ),
      true
    ),
    (
      """((while0 10.0 (block )) (foo == bar))""",
      ProgramWE.Prog(
        decls = List(),
        stmts = List(
          StmtWE.While(
            guard = ExprWE.Num(10.0),
            body = BlockWE.Err(BlockErr.ManyNoStmts)
          )
        ),
        expr = ExprWE.BinOpExpr(ExprWE.Var("foo"), BinOp.Equals, ExprWE.Var("bar"))
      ),
      true
    ),
    (
      "a",
      ProgramWE.Err(ProgErr.NotAList),
      true
    ),
    (
      "((1.0 = 1.0))",
      ProgramWE.Prog(
        decls = List(),
        stmts = List(),
        expr = ExprWE.Err(ExprErr.BadOperand)
      ),
      true
    ),
    (
      "((1.0 == 1.0))",
      ProgramWE.Prog(
        decls = List(),
        stmts = List(),
        expr = ExprWE.BinOpExpr(
          ExprWE.VarErrNode(VarErr.NotAName),
          BinOp.Equals,
          ExprWE.VarErrNode(VarErr.NotAName)
        )
      ),
      true
    ),
    (
      "((def x 1.0) (x = 0.0) (def y x) y)",
       ProgramWE.Prog(
        decls = List(
          DeclWE.Def(
            ExprWE.Var("x"),
            ExprWE.Num(1.0)
          )
        ),
        stmts = List(
          StmtWE.Assign(
            ExprWE.Var("x"),
            ExprWE.Num(0.0)
          ),
          StmtWE.Err(
            StmtErr.DeclAtStmtPosition
          )
        ),
        expr = ExprWE.Var("y")
      ),
      true
    ),
    (
      """((def x 0.0) (while0 x (x + 1.0)) (foo == bar))""",
      ProgramWE.Prog(
        decls = List(
          DeclWE.Def(
            ExprWE.Var("x"),
            ExprWE.Num(0.0)
          )
        ),
        stmts = List(
          StmtWE.While(
            guard = ExprWE.Var("x"),
            body = BlockWE.One(
              StmtWE.Err(
                StmtErr.Malformed
              )
            )
          )
        ),
        expr = ExprWE.BinOpExpr(ExprWE.Var("foo"), BinOp.Equals, ExprWE.Var("bar"))
      ),
      true
    ),
    (
      "((while0 ) 1.0)",
      ProgramWE.Prog(
        decls = List(),
        stmts = List(
          StmtWE.Err(StmtErr.WhileMalformed)
        ),
        expr = ExprWE.Num(1.0)
      ),
      true
    ),
  )