package test

import munit.FunSuite
import main.MainFuncs
import static.Parser
import ast._
import ast.ParseErrNodes._
import ast.ConverterToClean.progToClean
import ast.ConverterToClean.rawSystemToClean

class ParserTests extends FunSuite {
  
  def processTypedSysParse(input : String) : (RawSystemWE, Boolean) =
    val inputSexp = MainFuncs.readSexp(input);
    val system    = Parser.parseTypedSys(inputSexp); 
    val hasError  = rawSystemToClean(system).isEmpty;
    (system, hasError)

  def processProgParse(input : String) : (ProgramWE, Boolean) = 
    val inputSexp = MainFuncs.readSexp(input);
    val prog      = Parser.parseProg(inputSexp); 
    val hasError  = progToClean(prog).isEmpty;
    (prog, hasError)

  def processSysParse(input : String) : (RawSystemWE, Boolean) = 
    val inputSexp = MainFuncs.readSexp(input);
    val system    = Parser.parseMixedSys(inputSexp); 
    val hasError  = rawSystemToClean(system).isEmpty;
    (system, hasError)

  val allTests = Seq(
    ("Typed System", processTypedSysParse, ParserTests.validTypedModuleCases, ParserTests.invalidTypedModuleCases),
    ("System",       processSysParse,      ParserTests.validModuleCases,      ParserTests.invalidModuleCases),
    ("Prog",         processProgParse,     ParserTests.validClassCases,       ParserTests.invalidClassCases)
  )

  allTests.foreach { (inputKindName, processorFun, validCases, invalidCases) =>

    validCases.foreach{ case (input, expected) =>
      test(s"Well-formed $inputKindName: Parse + hasError test for input: $input") {
        val (parsed, hasError) = processorFun(input)
        assertEquals(parsed, expected)
        assertEquals(hasError, false)
      }
    }

    invalidCases.foreach{ case (input, expected) =>
      test(s"Ill-formed $inputKindName: Parse + hasError test for input: $input") {
        val (parsed, hasError) = processorFun(input)
        assertEquals(parsed, expected)
        assertEquals(hasError, true)
      }
    }
  }

}

object ParserTests:

  val validTypedModuleCases = Seq(
  (
    """
    (
    (tmodule A (class Point (x y) 
        (method addCoords ()
          (def tempX (this --> x))
          (def tempY (this --> y))
          (tempX + tempY)  
        )) 
      (((x Number) (y Number)) ((addCoords () Number))))
    4.0
  )
    """,
    WE.Node(
        RawSystem(
          modules = List(
            WE.Node(Module(
              mname = WE.Node("A"),
              imports = List(),
              clas = 
                WE.Node(Class(
                  cname = WE.Node("Point"),
                  fields = List(WE.Node("x"), WE.Node("y")),
                  methods = List(
                    WE.Node(
                      Method(
                        mname = WE.Node("addCoords"),
                        params = Nil,
                        progb = WE.Node(ProgBlock(
                          decls = List(
                            WE.Node(
                              Decl(
                                varDecl = WE.Node("tempX"),
                                rhs = WE.Node(
                                  Expr.GetField(
                                    instance = WE.Node("this"),
                                    field = WE.Node("x")
                                  )
                                )
                              )
                            ),
                            WE.Node(
                              Decl(
                                varDecl = WE.Node("tempY"),
                                rhs = WE.Node(
                                  Expr.GetField(
                                    instance = WE.Node("this"),
                                    field = WE.Node("y")
                                  )
                                )
                              )
                            )
                          ),
                          stmts = Nil,
                          expr = WE.Node(
                            Expr.BinOpExpr(
                              lhs = WE.Node("tempX"),
                              op = BinOp.Add,
                              rhs = WE.Node("tempY")
                            )
                          ))
                      )
                    ))
                  ),
                  shape= Some(WE.Node(Type.Shape(
                    fieldTypes = List(WE.Node(FieldType(WE.Node("x"), WE.Node(Type.Number()))), WE.Node(FieldType(WE.Node("y"), WE.Node(Type.Number())))),
                    methodTypes = List(WE.Node(MethodType(WE.Node("addCoords"), List(), WE.Node(Type.Number()))))
                  )))
                )
              ), 
            ))
          ),
          imports = List(),
          progb = WE.Node(ProgBlock(
            decls = Nil,
            stmts = Nil,
            expr = WE.Node(Expr.Num(4.0))
          )
        ))
      )
  )
)

  val invalidTypedModuleCases = Seq(
    (
      """(
      (tmodule  A (class Point (x y)))
      (module A (class Point (x y)))
      4.0
      )""",
      WE.Node(
        RawSystem(
          modules = List(
            WE.Err(ModuleMalformed)
          ),
          imports = Nil,
          progb = WE.Node(ProgBlock(
            decls = Nil,
            stmts = List(WE.Err(StmtMalformed)),
            expr = WE.Node(Expr.Num(4.0))
          )
        ))
      )
    ),
    (
      """(
      (tmodule A (class Point (x y)) ((x Number) (y Number) ()))
      (tmodule B (class Point (x y) 
          (method addCoords ()
            (def tempX (this --> x))
            (def tempY (this --> y))
            (tempX + tempY)  
          )) 
       (((x) (y Number)) ((addCoords Number))))
      4.0
      )""",
      WE.Node(
        RawSystem(
          modules = List(
            WE.Node(Module(
              mname = WE.Node("A"),
              imports = List(),
              clas = 
                WE.Node(Class(
                  cname = WE.Node("Point"),
                  fields = List(WE.Node("x"), WE.Node("y")),
                  methods = List(),
                  shape = Some(WE.Err(ShapeMalformed))
                )
              ),
              
            )),
            WE.Node(Module(
              mname = WE.Node("B"),
              imports = List(),
              clas = 
                WE.Node(Class(
                  cname = WE.Node("Point"),
                  fields = List(WE.Node("x"), WE.Node("y")),
                  methods = List(
                    WE.Node(
                      Method(
                        mname = WE.Node("addCoords"),
                        params = Nil,
                        progb = WE.Node(ProgBlock(
                          decls = List(
                            WE.Node(
                              Decl(
                                varDecl = WE.Node("tempX"),
                                rhs = WE.Node(
                                  Expr.GetField(
                                    instance = WE.Node("this"),
                                    field = WE.Node("x")
                                  )
                                )
                              )
                            ),
                            WE.Node(
                              Decl(
                                varDecl = WE.Node("tempY"),
                                rhs = WE.Node(
                                  Expr.GetField(
                                    instance = WE.Node("this"),
                                    field = WE.Node("y")
                                  )
                                )
                              )
                            )
                          ),
                          stmts = Nil,
                          expr = WE.Node(
                            Expr.BinOpExpr(
                              lhs = WE.Node("tempX"),
                              op = BinOp.Add,
                              rhs = WE.Node("tempY")
                            )
                          ))
                      )
                    ))
                  ),
                  shape = Some(WE.Node(Type.Shape(
                    fieldTypes = List(WE.Err(FieldTypeMalformed), WE.Node(FieldType(WE.Node("y"), WE.Node(Type.Number())))),
                    methodTypes = List(WE.Err(MethodTypeMalformed))
                  )))
                )
              ),
            ))
          ),
          imports = List(),
          progb = WE.Node(ProgBlock(
            decls = Nil,
            stmts = Nil,
            expr = WE.Node(Expr.Num(4.0))
          )
        ))
      )
    )
  )

  val validModuleCases = Seq(
    (
      "((foo = 123.4) bar)",
      WE.Node(RawSystem(
        modules = List(),
        imports = List(),
        progb = WE.Node(ProgBlock(
          decls = List(),
          stmts = List(WE.Node(Stmt.Assign(
            lhs = WE.Node("foo"), 
            rhs = WE.Node(Expr.Num(123.4))
          ))),
          expr = WE.Node(Expr.Var(WE.Node("bar")))
        ))
      ))
    ),
    (
      """(
      (module A 
        (class Point (x y) 
          (method addCoords ()
            (def tempX (this --> x))
            (def tempY (this --> y))
            (tempX + tempY)  
          )
        )
      )
      (import A)
      (def x 3.0)
      (def y 2.0)
      (def pointA (new Point (x y)))
      (pointA --> addCoords ())
      )""",
      WE.Node(
        RawSystem(
          modules = List(
            WE.Node(Module(
              mname = WE.Node("A"),
              imports = List(),
              clas = 
                WE.Node(Class(
                  cname = WE.Node("Point"),
                  fields = List(WE.Node("x"), WE.Node("y")),
                  methods = List(
                    WE.Node(
                      Method(
                        mname = WE.Node("addCoords"),
                        params = Nil,
                        progb = WE.Node(ProgBlock(
                          decls = List(
                            WE.Node(
                              Decl(
                                varDecl = WE.Node("tempX"),
                                rhs = WE.Node(
                                  Expr.GetField(
                                    instance = WE.Node("this"),
                                    field = WE.Node("x")
                                  )
                                )
                              )
                            ),
                            WE.Node(
                              Decl(
                                varDecl = WE.Node("tempY"),
                                rhs = WE.Node(
                                  Expr.GetField(
                                    instance = WE.Node("this"),
                                    field = WE.Node("y")
                                  )
                                )
                              )
                            )
                          ),
                          stmts = Nil,
                          expr = WE.Node(
                            Expr.BinOpExpr(
                              lhs = WE.Node("tempX"),
                              op = BinOp.Add,
                              rhs = WE.Node("tempY")
                            )
                          ))
                      )
                    ))
                  ),
                  None
                )
              )
            ))
          ),
          imports = List(WE.Node(Import.Untyped(WE.Node("A")))),
          progb = WE.Node(ProgBlock(
            decls = List(
              WE.Node(
                Decl(
                  varDecl = WE.Node("x"),
                  rhs = WE.Node(Expr.Num(3.0))
                )
              ),
              WE.Node(
                Decl(
                  varDecl = WE.Node("y"),
                  rhs = WE.Node(Expr.Num(2.0))
                )
              ),
              WE.Node(
                Decl(
                  varDecl = WE.Node("pointA"),
                  rhs = WE.Node(
                    Expr.NewInstance(
                      cname = WE.Node("Point"),
                      args = List(
                        WE.Node("x"),
                        WE.Node("y")
                      )
                    )
                  )
                )
              )
            ),
            stmts = Nil,
            expr = WE.Node(
              Expr.CallMethod(
                instance = WE.Node("pointA"),
                method = WE.Node("addCoords"),
                args = Nil
              )
            )
          )
        ))
      )
    ),
    (
      """(
      (module A (class Point (x y)))
      (module B (import A) (class Point (x y)))
      (import B)
      4.0
      )""",
      WE.Node(
        RawSystem(
          modules = List(
            WE.Node(Module(
              mname = WE.Node("A"),
              imports = List(),
              clas = 
                WE.Node(Class(
                  cname = WE.Node("Point"),
                  fields = List(WE.Node("x"), WE.Node("y")),
                  methods = List(),
                  None
                )
              )
            )),
            WE.Node(Module(
              mname = WE.Node("B"),
              imports = List(WE.Node(Import.Untyped(WE.Node("A")))),
              clas = 
                WE.Node(Class(
                  cname = WE.Node("Point"),
                  fields = List(WE.Node("x"), WE.Node("y")),
                  methods = List(),
                  None
                )
              )
            )),
          ),
          imports = List(WE.Node(Import.Untyped(WE.Node("B")))),
          progb = WE.Node(ProgBlock(
            decls = Nil,
            stmts = Nil,
            expr = WE.Node(Expr.Num(4.0))
          )
        ))
      )
    )
  )

  val invalidModuleCases = Seq(
    (
      "()",
      WE.Err(SystemEmptyList)
    ),
    (
      "413.0",
      WE.Err(SystemNotAList)
    ),
    (
      """(
      (module)
      (module A (class Point (x y)))
      (module B (import A))
      (import B)
      (import A B)
      4.0
      )""",
      WE.Node(
        RawSystem(
          modules = List(
            WE.Err(ModuleMalformed),
            WE.Node(Module(
              mname = WE.Node("A"),
              imports = List(),
              clas = 
                WE.Node(Class(
                  cname = WE.Node("Point"),
                  fields = List(WE.Node("x"), WE.Node("y")),
                  methods = List(),
                  None
                )
              )
            )),
            WE.Err(ModuleMalformed)
          ),
          imports = List(WE.Node(Import.Untyped(WE.Node("B"))), WE.Err(ImportMalformed)),
          progb = WE.Node(ProgBlock(
            decls = Nil,
            stmts = Nil,
            expr = WE.Node(Expr.Num(4.0))
          )
        ))
      )
    )
  )

  val validClassCases = Seq(
    (
      "((foo = 123.4) bar)",
      WE.Node(Program(
        clss = List(),
        progb = WE.Node(ProgBlock(
          decls = List(),
          stmts = List(WE.Node(Stmt.Assign(
            lhs = WE.Node("foo"), 
            rhs = WE.Node(Expr.Num(123.4))
          ))),
          expr = WE.Node(Expr.Var(WE.Node("bar")))
        ))
      ))
    ),
    (
      """((if0 bar (block (baz = 1.0)) (block (qux = -2.3))) foo)""",
      WE.Node(Program(
        clss = List(),
        progb = WE.Node(ProgBlock(
          decls = List(),
          stmts = List(
            WE.Node(Stmt.Ifelse(
              WE.Node(Expr.Var(WE.Node("bar"))),
              WE.Node(StmtBlock.Many(
                List(),
                List(
                  WE.Node(Stmt.Assign(
                    lhs = WE.Node("baz"),
                    rhs = WE.Node(Expr.Num(1.0))
                  ))
                )
              )),
              WE.Node(StmtBlock.Many(
                List(),
                List(
                  WE.Node(Stmt.Assign(
                    WE.Node("qux"),
                    WE.Node(Expr.Num(-2.3))
                  ))
                )
              ))
            )
          )),
          expr = WE.Node(Expr.Var(WE.Node("foo")))
        ))))
    ),
    (
      """((while0 10.0 (block (foo = 10.0) (bar = -5.5))) (foo == bar))""",
      WE.Node(Program(
        clss = List(),
        progb = WE.Node(ProgBlock(
          decls = List(),
          stmts = List(
            WE.Node(Stmt.While(
              guard = WE.Node(Expr.Num(10.0)),
              body = WE.Node(StmtBlock.Many(
                  List(),
                stmts = List(
                  WE.Node(Stmt.Assign(
                    WE.Node("foo"),
                    WE.Node(Expr.Num(10.0))
                  )),
                  WE.Node(Stmt.Assign(
                    WE.Node("bar"),
                    WE.Node(Expr.Num(-5.5))
                  ))
                )
              )
            ))
          )),
          expr = WE.Node(Expr.BinOpExpr(
            WE.Node("foo"), 
            BinOp.Equals, 
            WE.Node("bar")
          )))
      )))
    ),
    (
      """((foo = (bar + baz)) (if0 qux (block (baz = 1.0)) (block (foo = -0.5))) bar)""",
      WE.Node(Program(
        clss = List(),
        progb = WE.Node(ProgBlock(
          decls = List(),
          stmts = List(
            WE.Node(Stmt.Assign(
              lhs = WE.Node("foo"),
              rhs = WE.Node(Expr.BinOpExpr(
                WE.Node("bar"), 
                BinOp.Add, 
                WE.Node("baz")
              ))
            )),
            WE.Node(Stmt.Ifelse(
              guard = WE.Node(Expr.Var(WE.Node("qux"))),
              tbranch = WE.Node(StmtBlock.Many(
                  List(),
                List(
                  WE.Node(Stmt.Assign(
                    WE.Node("baz"),
                    WE.Node(Expr.Num(1.0))
                  ))
                )
              )),
              ebranch = WE.Node(StmtBlock.Many(
                  List(),
                List(
                  WE.Node(Stmt.Assign(
                    WE.Node("foo"),
                    WE.Node(Expr.Num(-0.5))
                  ))
                )
              ))
            ))
          ),
          expr = WE.Node(Expr.Var(WE.Node("bar")))
        )
      )))
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
                cname = WE.Node("Point"),
                fields = List(WE.Node("x"), WE.Node("y")),
                methods = List(
                  WE.Node(
                    Method(
                      mname = WE.Node("addCoords"),
                      params = Nil,
                      progb = WE.Node(ProgBlock(
                        decls = List(
                          WE.Node(
                            Decl(
                              varDecl = WE.Node("tempX"),
                              rhs = WE.Node(
                                Expr.GetField(
                                  instance = WE.Node("this"),
                                  field = WE.Node("x")
                                )
                              )
                            )
                          ),
                          WE.Node(
                            Decl(
                              varDecl = WE.Node("tempY"),
                              rhs = WE.Node(
                                Expr.GetField(
                                  instance = WE.Node("this"),
                                  field = WE.Node("y")
                                )
                              )
                            )
                          )
                        ),
                        stmts = Nil,
                        expr = WE.Node(
                          Expr.BinOpExpr(
                            lhs = WE.Node("tempX"),
                            op = BinOp.Add,
                            rhs = WE.Node("tempY")
                          )
                        ))
                    ))
                  )
                ),
                None
              )
            )
          ),
          progb = WE.Node(ProgBlock(
            decls = List(
              WE.Node(
                Decl(
                  varDecl = WE.Node("x"),
                  rhs = WE.Node(Expr.Num(3.0))
                )
              ),
              WE.Node(
                Decl(
                  varDecl = WE.Node("y"),
                  rhs = WE.Node(Expr.Num(2.0))
                )
              ),
              WE.Node(
                Decl(
                  varDecl = WE.Node("pointA"),
                  rhs = WE.Node(
                    Expr.NewInstance(
                      cname = WE.Node("Point"),
                      args = List(
                        WE.Node("x"),
                        WE.Node("y")
                      )
                    )
                  )
                )
              )
            ),
            stmts = Nil,
            expr = WE.Node(
              Expr.CallMethod(
                instance = WE.Node("pointA"),
                method = WE.Node("addCoords"),
                args = Nil
              )
            )
          )
        ))
      )
    ), 
  )

  val invalidClassCases = Seq(
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
        progb = WE.Node(ProgBlock(
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
          expr = WE.Err(ExprMalformed) )
      )))
    ),
    (
      """
      ((foo = ) =)
      """,
      WE.Node(Program(
        clss = List(),
        progb = WE.Node(ProgBlock(
          decls = List(),
          stmts = List(
            WE.Err(AssignRhsMalformed)
          ),
          expr = WE.Node(Expr.Var(WE.Err(NameIsKeyword))))
      )))
    ),
    (
      "()",
      WE.Err(ProgEmptyList)
    ),
    (
      "((def x 1.0) (def y 2.0) (def x) (x + y))",
      WE.Node(Program(
        clss = List(),
        progb = WE.Node(ProgBlock(
          decls = List(
            WE.Node(Decl(
              WE.Node("x"),
              WE.Node(Expr.Num(1.0))
            )),
            WE.Node(Decl(
              WE.Node("y"),
              WE.Node(Expr.Num(2.0))
            )),
            WE.Err(DeclMalformed),
          ),
          stmts = List(),
          WE.Node(Expr.BinOpExpr(
            WE.Node("x"),
            BinOp.Add,
            WE.Node("y"))))
      )))
    ),
    (
      """((while0 10.0 (block )) (foo == bar))""",
      WE.Node(Program(
        clss = List(),
        progb = WE.Node(ProgBlock(
          decls = List(),
          stmts = List(
            WE.Node(Stmt.While(
              guard = WE.Node(Expr.Num(10.0)),
              body = WE.Err(BlockManyNoStmts)
            ))
          ),
          expr = WE.Node(Expr.BinOpExpr(
            WE.Node("foo"), 
            BinOp.Equals, 
            WE.Node("bar")
          ))
      ))))
    ),
    (
      "a",
      WE.Err(ProgNotAList)
    ),
    (
      "((1.0 = 1.0))",
      WE.Node(Program(
        clss = List(),
        progb = WE.Node(ProgBlock(
          decls = List(),
          stmts = List(),
          expr = WE.Err(ExprBadOperand))
      )))
    ),
    (
      "((1.0 == 1.0))",
      WE.Node(Program(
        clss = List(),
        progb = WE.Node(ProgBlock(
          decls = List(),
          stmts = List(),
          expr = WE.Node(Expr.BinOpExpr(
            WE.Err(NotAName),
            BinOp.Equals,
            WE.Err(NotAName)
          )))
      )))
    ),
    (
      "((def x 1.0) (x = 0.0) (def y x) y)",
       WE.Node(Program(
        clss = List(),
        progb = WE.Node(ProgBlock(
          decls = List(
            WE.Node(Decl(
              WE.Node("x"),
              WE.Node(Expr.Num(1.0))
            ))
          ),
          stmts = List(
            WE.Node(Stmt.Assign(
              WE.Node("x"),
              WE.Node(Expr.Num(0.0))
            )),
            WE.Err(DeclAtStmtPosition)
          ),
          expr = WE.Node(Expr.Var(WE.Node("y")))
        )
      )))
    ),
    (
      """((def x 0.0) (while0 x (x + 1.0)) (foo == bar))""",
      WE.Node(Program(
        clss = List(),
        progb = WE.Node(ProgBlock(
          decls = List(
            WE.Node(Decl(
              WE.Node("x"),
              WE.Node(Expr.Num(0.0))
            ))
          ),
          stmts = List(
            WE.Node(Stmt.While(
              guard = WE.Node(Expr.Var(WE.Node("x"))),
              body = WE.Node(
                StmtBlock.One(WE.Err(StmtMalformed)))
            ))
          ),
          expr = WE.Node(Expr.BinOpExpr(
            WE.Node("foo"), 
            BinOp.Equals, 
            WE.Node("bar")
          )))
      )))
    ),
    (
      "((while0 0.3 (block (def x -3.0))) 1.0)",
      WE.Node(Program(
        clss = List(),
        progb = WE.Node(ProgBlock(
          decls = List(),
          stmts = List(
            WE.Node(Stmt.While(
              guard = WE.Node(Expr.Num(0.3)),
              body = WE.Err(BlockManyNoStmts)
            ))
          ),
          expr = WE.Node(Expr.Num(1.0))
        )
      )))
    ),
    (
      "((while0 ) 1.0)",
      WE.Node(Program(
        clss = List(),
        progb = WE.Node(ProgBlock(
          decls = List(),
          stmts = List(
            WE.Err(WhileMalformed)
          ),
          expr = WE.Node(Expr.Num(1.0))
        )
      )))
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
          progb = WE.Node(ProgBlock(
            decls = List(
              WE.Node(
                Decl(
                  varDecl = WE.Node("clark"),
                  rhs = WE.Node(
                    Expr.NewInstance(
                      cname = WE.Node("Point"),
                      args = List()
                    )
                  )
                )
              )
            ),
            stmts = List(),
            expr = WE.Node(
              Expr.IsInstanceOf(
                instance = WE.Node("clark"),
                cname = WE.Node("Point")
              )
            )
          )
        ))
      )
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
                cname = WE.Node("Point"),
                fields = List(
                  WE.Node("x")
                ),
                methods = List(
                  WE.Err(MethodMalformed),
                  WE.Node(
                    Method(
                      mname = WE.Node("addy"),
                      params = List(
                        WE.Node("y")
                      ),
                      progb = WE.Node(ProgBlock(
                        decls = List(),
                        stmts = List(),
                        expr = WE.Node(
                          Expr.BinOpExpr(
                            lhs = WE.Node("x"),
                            op = BinOp.Add,
                            rhs = WE.Node("y")
                          )
                        ))
                    ))
                  )
                ),
                None
              )
            )
          ),
          progb = WE.Node(ProgBlock(
            decls = List(
              WE.Node(
                Decl(
                  varDecl = WE.Node("this"),
                  rhs = WE.Node(Expr.Num(2.0))
                )
              ),
              WE.Node(
                Decl(
                  varDecl = WE.Node("three"),
                  rhs = WE.Node(Expr.Num(3.0))
                )
              ),
              WE.Node(
                Decl(
                  varDecl = WE.Err(NameIsKeyword),
                  rhs = WE.Node(
                    Expr.NewInstance(
                      cname = WE.Node("Point"),
                      args = List(
                        WE.Node("this")
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
                  field = WE.Node("x"),
                  rhs = WE.Node(Expr.Num(4.0))
                )
              )
            ),
            expr = WE.Node(
              Expr.CallMethod(
                instance = WE.Err(NameIsKeyword),
                method = WE.Node("addy"),
                args = List(
                  WE.Err(NotAName)
                )
              )
            )
          )
       ))
      )
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
                cname = WE.Node("Point"),
                fields = List(WE.Node("x")),
                methods = List(WE.Node(Method(WE.Node("addy"), List(WE.Node("y")), WE.Err(ProgBlockNoExpr)))),
                None
              )
            )
          ),
          progb = WE.Node(ProgBlock(
            decls = List(),
            stmts = List(),
            expr = WE.Node(Expr.Num(2.0)))
       ))
      )
    )
  )
