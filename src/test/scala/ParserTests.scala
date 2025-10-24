package test

import munit.FunSuite
import main.MainFuncs
import static.Parser
import ast._
import ast.ParseErrNodes._
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
      WE.Node(Program(
        clss = List(),
        decls = List(),
        stmts = List(WE.Node(Stmt.Assign(
          lhs = WE.Node(VarRef("foo")), 
          rhs = WE.Node(Expr.Num(123.4))
        ))),
        expr = WE.Node(Expr.Var(WE.Node(VarRef("bar"))))
      )),
      false
    ),
    (
      """((if0 bar (block (baz = 1.0)) (block (qux = -2.3))) foo)""",
      WE.Node(Program(
        clss = List(),
        decls = List(),
        stmts = List(
          WE.Node(Stmt.Ifelse(
            WE.Node(Expr.Var(WE.Node(VarRef("bar")))),
            WE.Node(Block.Many(
              List(),
              List(
                WE.Node(Stmt.Assign(
                  lhs = WE.Node(VarRef("baz")),
                  rhs = WE.Node(Expr.Num(1.0))
                ))
              )
            )),
            WE.Node(Block.Many(
              List(),
              List(
                WE.Node(Stmt.Assign(
                  WE.Node(VarRef("qux")),
                  WE.Node(Expr.Num(-2.3))
                ))
              )
            ))
          )
        )),
        expr = WE.Node(Expr.Var(WE.Node(VarRef("foo"))))
      )),
      false
    ),
    (
      """((while0 10.0 (block (foo = 10.0) (bar = -5.5))) (foo == bar))""",
      WE.Node(Program(
        clss = List(),
        decls = List(),
        stmts = List(
          WE.Node(Stmt.While(
            guard = WE.Node(Expr.Num(10.0)),
            body = WE.Node(Block.Many(
                List(),
              stmts = List(
                WE.Node(Stmt.Assign(
                  WE.Node(VarRef("foo")),
                  WE.Node(Expr.Num(10.0))
                )),
                WE.Node(Stmt.Assign(
                  WE.Node(VarRef("bar")),
                  WE.Node(Expr.Num(-5.5))
                ))
              )
            )
          ))
        )),
        expr = WE.Node(Expr.BinOpExpr(
          WE.Node(VarRef("foo")), 
          BinOp.Equals, 
          WE.Node(VarRef("bar"))
        ))
      )),
      false
    ),
    (
      """((foo = (bar + baz)) (if0 qux (block (baz = 1.0)) (block (foo = -0.5))) bar)""",
      WE.Node(Program(
        clss = List(),
        decls = List(),
        stmts = List(
          WE.Node(Stmt.Assign(
            lhs = WE.Node(VarRef("foo")),
            rhs = WE.Node(Expr.BinOpExpr(
              WE.Node(VarRef("bar")), 
              BinOp.Add, 
              WE.Node(VarRef("baz"))
            ))
          )),
          WE.Node(Stmt.Ifelse(
            guard = WE.Node(Expr.Var(WE.Node(VarRef("qux")))),
            tbranch = WE.Node(Block.Many(
                List(),
              List(
                WE.Node(Stmt.Assign(
                  WE.Node(VarRef("baz")),
                  WE.Node(Expr.Num(1.0))
                ))
              )
            )),
            ebranch = WE.Node(Block.Many(
                List(),
              List(
                WE.Node(Stmt.Assign(
                  WE.Node(VarRef("foo")),
                  WE.Node(Expr.Num(-0.5))
                ))
              )
            ))
          ))
        ),
        expr = WE.Node(Expr.Var(WE.Node(VarRef("bar"))))
      )),
      false
    ),
    (
      """(
      (class Point (x y) 
        (method addCoords ()
          (def tempX (this --> x))
          (def tempY (this --> y))
          (tempX + tempY)  
        )
      )
      (def x 3.0)
      (def y 2.0)
      (def pointA (new Point (x y)))
      (pointA --> addCoords ())
      )""",
      WE.Node(
        Program(
          clss = List(
            WE.Node(
              Class(
                cname = WE.Node(Name("Point")),
                fields = List(WE.Node(Name("x")), WE.Node(Name("y"))),
                methods = List(
                  WE.Node(
                    Method(
                      mname = WE.Node(Name("addCoords")),
                      params = Nil,
                      decls = List(
                        WE.Node(
                          Decl(
                            varDecl = WE.Node(Name("tempX")),
                            rhs = WE.Node(
                              Expr.GetField(
                                instance = WE.Node(VarRef("this")),
                                field = WE.Node(Name("x"))
                              )
                            )
                          )
                        ),
                        WE.Node(
                          Decl(
                            varDecl = WE.Node(Name("tempY")),
                            rhs = WE.Node(
                              Expr.GetField(
                                instance = WE.Node(VarRef("this")),
                                field = WE.Node(Name("y"))
                              )
                            )
                          )
                        )
                      ),
                      stmts = Nil,
                      expr = WE.Node(
                        Expr.BinOpExpr(
                          lhs = WE.Node(VarRef("tempX")),
                          op = BinOp.Add,
                          rhs = WE.Node(VarRef("tempY"))
                        )
                      )
                    )
                  )
                )
              )
            )
          ),
          decls = List(
            WE.Node(
              Decl(
                varDecl = WE.Node(Name("x")),
                rhs = WE.Node(Expr.Num(3.0))
              )
            ),
            WE.Node(
              Decl(
                varDecl = WE.Node(Name("y")),
                rhs = WE.Node(Expr.Num(2.0))
              )
            ),
            WE.Node(
              Decl(
                varDecl = WE.Node(Name("pointA")),
                rhs = WE.Node(
                  Expr.NewInstance(
                    cname = WE.Node(Name("Point")),
                    args = List(
                      WE.Node(VarRef("x")),
                      WE.Node(VarRef("y"))
                    )
                  )
                )
              )
            )
          ),
          stmts = Nil,
          expr = WE.Node(
            Expr.CallMethod(
              instance = WE.Node(VarRef("pointA")),
              method = WE.Node(Name("addCoords")),
              args = Nil
            )
          )
        )
      ),
      false
    )
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
      WE.Node(Program(
        clss = List(),
        decls = List(),
        stmts = List(
          WE.Err(StmtMalformed),
          WE.Err(StmtMalformed),
          WE.Err(StmtMalformed),
          WE.Err(StmtMalformed),
          WE.Err(StmtMalformed),
          WE.Err(StmtMalformed),
          WE.Err(StmtMalformed)
        ),
        expr = WE.Err(ExprMalformed) 
      )),
      true
    ),
    (
      """
      ((foo = ) =)
      """,
      WE.Node(Program(
        clss = List(),
        decls = List(),
        stmts = List(
          WE.Err(AssignRhsMalformed)
        ),
        expr = WE.Node(Expr.Var(WE.Err(NameIsKeyword)))
      )),
      true
    ),
    (
      "()",
      WE.Err(ProgEmptyList),
      true
    ),
    (
      "((def x 1.0) (def y 2.0) (def x) (x + y))",
      WE.Node(Program(
        clss = List(),
        decls = List(
          WE.Node(Decl(
            WE.Node(Name("x")),
            WE.Node(Expr.Num(1.0))
          )),
          WE.Node(Decl(
            WE.Node(Name("y")),
            WE.Node(Expr.Num(2.0))
          )),
          WE.Err(DeclMalformed),
        ),
        stmts = List(),
        WE.Node(Expr.BinOpExpr(
          WE.Node(VarRef("x")),
          BinOp.Add,
          WE.Node(VarRef("y"))))
      )),
      true
    ),
    (
      """((while0 10.0 (block )) (foo == bar))""",
      WE.Node(Program(
        clss = List(),
        decls = List(),
        stmts = List(
          WE.Node(Stmt.While(
            guard = WE.Node(Expr.Num(10.0)),
            body = WE.Err(BlockManyNoStmts)
          ))
        ),
        expr = WE.Node(Expr.BinOpExpr(
          WE.Node(VarRef("foo")), 
          BinOp.Equals, 
          WE.Node(VarRef("bar"))
        ))
      )),
      true
    ),
    (
      "a",
      WE.Err(ProgNotAList),
      true
    ),
    (
      "((1.0 = 1.0))",
      WE.Node(Program(
        clss = List(),
        decls = List(),
        stmts = List(),
        expr = WE.Err(ExprBadOperand)
      )),
      true
    ),
    (
      "((1.0 == 1.0))",
      WE.Node(Program(
        clss = List(),
        decls = List(),
        stmts = List(),
        expr = WE.Node(Expr.BinOpExpr(
          WE.Err(NotAName),
          BinOp.Equals,
          WE.Err(NotAName)
        ))
      )),
      true
    ),
    (
      "((def x 1.0) (x = 0.0) (def y x) y)",
       WE.Node(Program(
        clss = List(),
        decls = List(
          WE.Node(Decl(
            WE.Node(Name("x")),
            WE.Node(Expr.Num(1.0))
          ))
        ),
        stmts = List(
          WE.Node(Stmt.Assign(
            WE.Node(VarRef("x")),
            WE.Node(Expr.Num(0.0))
          )),
          WE.Err(DeclAtStmtPosition)
        ),
        expr = WE.Node(Expr.Var(WE.Node(VarRef("y"))))
      )),
      true
    ),
    (
      """((def x 0.0) (while0 x (x + 1.0)) (foo == bar))""",
      WE.Node(Program(
        clss = List(),
        decls = List(
          WE.Node(Decl(
            WE.Node(Name("x")),
            WE.Node(Expr.Num(0.0))
          ))
        ),
        stmts = List(
          WE.Node(Stmt.While(
            guard = WE.Node(Expr.Var(WE.Node(VarRef("x")))),
            body = WE.Node(
              Block.One(WE.Err(StmtMalformed)))
          ))
        ),
        expr = WE.Node(Expr.BinOpExpr(
          WE.Node(VarRef("foo")), 
          BinOp.Equals, 
          WE.Node(VarRef("bar"))
        ))
      )),
      true
    ),
    (
      "((while0 ) 1.0)",
      WE.Node(Program(
        clss = List(),
        decls = List(),
        stmts = List(
          WE.Err(WhileMalformed)
        ),
        expr = WE.Node(Expr.Num(1.0))
      )),
      true
    ),
    (
      """(
      (class Point)
      (def clark (new Point ()))
      (clark isa Point)
      )""",
      WE.Node(
        Program(
          clss = List(
            WE.Err(ClassMalformed)
          ),
          decls = List(
            WE.Node(
              Decl(
                varDecl = WE.Node(Name("clark")),
                rhs = WE.Node(
                  Expr.NewInstance(
                    cname = WE.Node(Name("Point")),
                    args = List()
                  )
                )
              )
            )
          ),
          stmts = List(),
          expr = WE.Node(
            Expr.IsInstanceOf(
              instance = WE.Node(VarRef("clark")),
              cname = WE.Node(Name("Point"))
            )
          )
        )
      ),
      true
    ),
    (
      """
      (
      (class Point (x) 
        (method dummy x)
        (method addy (y) (x + y))
      )
      (def this 2.0)
      (def three 3.0)
      (def new (new Point (this)))
      (new --> x = 4.0)
      (new --> addy (3.0))
      )
      """,
      WE.Node(
        Program(
          clss = List(
            WE.Node(
              Class(
                cname = WE.Node(Name("Point")),
                fields = List(
                  WE.Node(Name("x"))
                ),
                methods = List(
                  WE.Err(MethodMalformed),
                  WE.Node(
                    Method(
                      mname = WE.Node(Name("addy")),
                      params = List(
                        WE.Node(Name("y"))
                      ),
                      decls = List(),
                      stmts = List(),
                      expr = WE.Node(
                        Expr.BinOpExpr(
                          lhs = WE.Node(VarRef("x")),
                          op = BinOp.Add,
                          rhs = WE.Node(VarRef("y"))
                        )
                      )
                    )
                  )
                )
              )
            )
          ),
          decls = List(
            WE.Node(
              Decl(
                varDecl = WE.Node(Name("this")),
                rhs = WE.Node(Expr.Num(2.0))
              )
            ),
            WE.Node(
              Decl(
                varDecl = WE.Node(Name("three")),
                rhs = WE.Node(Expr.Num(3.0))
              )
            ),
            WE.Node(
              Decl(
                varDecl = WE.Err(NameIsKeyword),
                rhs = WE.Node(
                  Expr.NewInstance(
                    cname = WE.Node(Name("Point")),
                    args = List(
                      WE.Node(VarRef("this"))
                    )
                  )
                )
              )
            )
          ),
          stmts = List(
            WE.Node(
              Stmt.FieldAssign(
                instance = WE.Err(NameIsKeyword),
                field = WE.Node(Name("x")),
                rhs = WE.Node(Expr.Num(4.0))
              )
            )
          ),
          expr = WE.Node(
            Expr.CallMethod(
              instance = WE.Err(NameIsKeyword),
              method = WE.Node(Name("addy")),
              args = List(
                WE.Err(NotAName)
              )
            )
          )
       )
      ),
      true
    ),
    (
      """
      (
      (class Point (x) 
        (method addy (y))
      )
      2.0
      )
      """,
      WE.Node(
        Program(
          clss = List(
            WE.Node(
              Class(
                cname = WE.Node(Name("Point")),
                fields = List(WE.Node(Name("x"))),
                methods = List(WE.Err(MethodNoExpr))
              )
            )
          ),
          decls = List(),
          stmts = List(),
          expr = WE.Node(Expr.Num(2.0))
       )
      ),
      true
    )
  )