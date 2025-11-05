package test

import munit.FunSuite
import main.MainFuncs
import ast._
import ast.ValidityErrNodes._
import ast.ConverterToClean.progToClean
import static.Parser
import static.{VCheckTLDups, VCheckMFPNameDups, VCheckUndefined}
import ast.ConverterToClean.systemToClean
import ast.Expr.Num

class ValidityTest extends FunSuite {

    def progWEOrClean(prog : ProgramWE) : Either[ProgramWE, CleanProgram] =
        progToClean(prog) match 
            case None            => Left(prog)
            case Some(cleanProg) => Right(cleanProg)
    
    def sysWEOrClean(sys : SystemWE) : Either[SystemWE, CleanSystem] =
        systemToClean(sys) match 
            case None            => Left(sys)
            case Some(cleanSys) => Right(cleanSys)

    def processProgTestCase(testTuple : (String, ProgramWE | CleanProgram, Boolean)) : Unit = testTuple match
        case (inputStr, expectedAST, isErrExpected) => 
            val progType = if isErrExpected then "Well-formed Invalid" else "Well-formed Valid"
            val testName = s"$progType Prog: validity pipeline for input: $inputStr"
            test(testName) {
                val inputSexp = MainFuncs.readSexp(inputStr)

                val pipeRes = 
                    for 
                        parsedProg <- progWEOrClean(Parser.parseProg(inputSexp))
                        vCheck1    <- progWEOrClean(VCheckTLDups.classDupsProg(parsedProg))
                        vCheck2    <- progWEOrClean(VCheckMFPNameDups.mfpDupsProg(vCheck1))
                        validPr    <- progWEOrClean(VCheckUndefined.closedProg(vCheck2))
                    yield 
                        validPr

                pipeRes match 
                    case Left(progWE)     => 
                        assertEquals(progWE, expectedAST)
                        assertEquals(true, isErrExpected)
                    case Right(cleanProg) => 
                        assertEquals(cleanProg, expectedAST)
                        assertEquals(false, isErrExpected)

            }

    def processSysTestCase(testTuple : (String, SystemWE | CleanSystem, Boolean)) : Unit = testTuple match
        case (inputStr, expectedAST, isErrExpected) => 
            val progType = if isErrExpected then "Well-formed Invalid" else "Well-formed Valid"
            val testName = s"$progType System: validity pipeline for input: $inputStr"
            test(testName){
                val inputSexp = MainFuncs.readSexp(inputStr)

                val pipeRes = 
                    for 
                        parsedSys <- sysWEOrClean(Parser.parseSys(inputSexp))
                        vCheck1   <- sysWEOrClean(VCheckTLDups.moduleDupsSys(parsedSys))
                        vCheck2   <- sysWEOrClean(VCheckMFPNameDups.mfpDupsSys(vCheck1))
                        validSys  <- sysWEOrClean(VCheckUndefined.closedSystem(vCheck2))
                    yield 
                        validSys

                pipeRes match 
                    case Left(sysWE)     => 
                        assertEquals(sysWE, expectedAST)
                        assertEquals(true, isErrExpected)
                    case Right(cleanSys) => 
                        assertEquals(cleanSys, expectedAST)
                        assertEquals(false, isErrExpected)

            }

    ValidityTests.validProgCases.foreach(processProgTestCase)
    ValidityTests.invalidProgCases.foreach(processProgTestCase)
    ValidityTests.validSysCases.foreach(processSysTestCase)
    ValidityTests.invalidSysCases.foreach(processSysTestCase)
}

object ValidityTests {
    val validProgCases = Seq(
    (
        "((def foo 123.4) (foo = 100.0) foo)",
        Program[Clean](
            clss = List(),
            progb = ProgBlock(
                decls = List(
                    Decl("foo", Expr.Num(123.4))
                ),
                stmts = List(
                    Stmt.Assign("foo", Expr.Num(100.0))
                ),
                expr = Expr.Var("foo"))
        ),
        false
    ),
    (
        "((def a 1.0) (def b 2.0) (a = (a + b)) (a / b))",
        Program[Clean](
            clss = List(),
            progb = ProgBlock(
                decls = List(
                    Decl("a", Expr.Num(1.0)),
                    Decl("b", Expr.Num(2.0))
                ),
                stmts = List(
                    Stmt.Assign(
                    lhs = "a",
                    rhs = Expr.BinOpExpr("a", BinOp.Add, "b")
                    )
                ),
                expr = Expr.BinOpExpr("a", BinOp.Div, "b")
            )
        ),
        false
    ),
    (
        "((def x 10.0) (if0 x (block (def y 2.0) (y = 5.0)) (block (def z 3.0) (z = 7.0))) x)",
        Program[Clean](
            clss = List(),
            progb = ProgBlock(
                decls = List(
                    Decl("x", Expr.Num(10.0))
                ),
                stmts = List(
                    Stmt.Ifelse(
                    guard = Expr.Var("x"),
                    tbranch = StmtBlock.Many(
                        decls = List(
                        Decl("y", Expr.Num(2.0))
                        ),
                        stmts = List(
                        Stmt.Assign("y", Expr.Num(5.0))
                        )
                    ),
                    ebranch = StmtBlock.Many(
                        decls = List(
                        Decl("z", Expr.Num(3.0))
                        ),
                        stmts = List(
                        Stmt.Assign("z", Expr.Num(7.0))
                        )
                    ))
                ),
                expr = Expr.Var("x")
            ),    
        ),
        false
    ),
    (
        "((def guard 1.0) (while0 guard (block (def i 0.0) (i = (i + guard)))) guard)",
        Program[Clean](
            clss = List(),
            progb = ProgBlock(
                decls = List(
                    Decl("guard", Expr.Num(1.0))
                ),
                stmts = List(
                    Stmt.While(
                    guard = Expr.Var("guard"),
                    body = StmtBlock.Many(
                        decls = List(
                        Decl("i", Expr.Num(0.0))
                        ),
                        stmts = List(
                        Stmt.Assign(
                            "i",
                            Expr.BinOpExpr("i", BinOp.Add, "guard")
                        )
                        )
                    )
                    )
                ),
                expr = Expr.Var("guard")
            )
        ),
        false
    ),
    (
        "((def a 10.0) (def b 20.0) (a = (a + b)) (if0 a (block (def c 1.0) (c = (a / b))) (block (def d 2.0) (d = (b / a)))) (a == b))",
        Program[Clean](
            clss = List(),
            progb = ProgBlock(
                decls = List(
                    Decl("a", Expr.Num(10.0)),
                    Decl("b", Expr.Num(20.0))
                ),
                stmts = List(
                    Stmt.Assign(
                    "a",
                    Expr.BinOpExpr("a", BinOp.Add,"b")
                    ),
                    Stmt.Ifelse(
                    guard = Expr.Var("a"),
                    tbranch = StmtBlock.Many(
                        decls = List(
                        Decl("c", Expr.Num(1.0))
                        ),
                        stmts = List(
                        Stmt.Assign(
                            "c",
                            Expr.BinOpExpr("a", BinOp.Div, "b")
                        )
                        )
                    ),
                    ebranch = StmtBlock.Many(
                        decls = List(
                        Decl("d", Expr.Num(2.0))
                        ),
                        stmts = List(
                        Stmt.Assign(
                            "d",
                            Expr.BinOpExpr("b", BinOp.Div, "a")
                        )
                        )
                    )
                    )
                ),
                expr = Expr.BinOpExpr("a", BinOp.Equals, "b")
            ) 
        ),
        false
    ),
    (
        "((class A ()) (class B (owo) (method uwu (num) (def foo 123.4) (foo = num) this) (method fave () (def o (new A ())) 413.0)) (def o (new A ())) (o isa A))",
        Program[Clean](
            clss = List(
                Class("A", List(), List()),
                Class(
                    "B", 
                    List("owo"),
                    List(
                        Method(
                            "uwu",
                            List("num"),
                            progb = ProgBlock(
                                List(Decl("foo", Expr.Num(123.4))), 
                                List(Stmt.Assign("foo", Expr.Var("num"))),
                                Expr.Var("this")
                            )
                        ),
                        Method(
                            "fave",
                            List(),
                            progb = ProgBlock(
                                List(Decl("o", Expr.NewInstance("A", List()))),
                                List(),
                                Expr.Num(413.0)
                            )
                        )
                    )
                )
            ),
            progb = ProgBlock(
                decls = List(
                    Decl("o", Expr.NewInstance("A", List()))
                ),
                stmts = List(),
                expr = Expr.IsInstanceOf("o", "A"))
        ),
        false
    ),
    (
        "((class B (owo) (method uwu (num) (def foo 123.4) (foo = num) this)) (def num 413.0) (def o (new B (num))) (o --> notafield = (o --> somemethod (num))) (o --> alsonotafield))",
        Program[Clean](
            clss = List(
                Class(
                    "B", 
                    List("owo"),
                    List(
                        Method(
                            "uwu",
                            List("num"),
                            progb = ProgBlock(
                                List(Decl("foo", Expr.Num(123.4))), 
                                List(Stmt.Assign("foo", Expr.Var("num"))),
                                Expr.Var("this"))
                        )
                    )
                )
            ),
            progb = ProgBlock(
                decls = List(
                    Decl("num", Expr.Num(413.0)),
                    Decl("o", Expr.NewInstance("B", List("num")))
                ),
                stmts = List(
                    Stmt.FieldAssign(
                        "o", 
                        "notafield", 
                        Expr.CallMethod("o", "somemethod", List("num")))
                ),
                expr = Expr.GetField("o", "alsonotafield"))
        ),
        false
    )   
    )

    val invalidProgCases = Seq(
    (
        "((class A ()) (class A ()) 413.0)",
        WE.Node(Program(
            clss = List(
                WE.Node(Class(WE.Node("A"), List(), List())),
                WE.Node(Class(WE.Err(DuplicateClassName), List(), List()))
            ),
            progb = WE.Node(ProgBlock(
                decls = List(),
                stmts = List(),
                expr = WE.Node(Expr.Num(413.0))
            ))
        )),
        true
    ),
    (
        "((class A (a)) (class B (a a) (method owo (a a) 612.0) (method owo () -413.0)) 413.0)",
        WE.Node(Program(
            clss = List(
                WE.Node(Class(WE.Node("A"), List(WE.Node("a")), List())),
                WE.Node(Class(WE.Node("B"), List(WE.Node("a"), WE.Err(DuplicateFieldName)), 
                List(
                    WE.Node(Method(
                        WE.Node("owo"),
                        List(WE.Node("a"), WE.Err(DuplicateParamName)),
                        progb = WE.Node(ProgBlock(
                            List(), 
                            List(),
                            WE.Node(Expr.Num(612.0))))
                    )),
                    WE.Node(Method(
                        WE.Err(DuplicateMethod),
                        List(),
                        progb = WE.Node(ProgBlock(
                            List(), 
                            List(),
                            WE.Node(Expr.Num(-413.0)))
                    )))
                )))
            ),
            progb = WE.Node(ProgBlock(
                decls = List(),
                stmts = List(),
                expr = WE.Node(Expr.Num(413.0))))
        )),
        true
    ),
    (
        "((class A (a)) (a + this))",
        WE.Node(Program(
            clss = List(
                WE.Node(Class(WE.Node("A"), List(WE.Node("a")), List()))
            ),
            progb = WE.Node(ProgBlock(
                decls = List(),
                stmts = List(),
                expr = 
                    WE.Node(Expr.BinOpExpr(
                        WE.Err(VarNotDeclared),
                        BinOp.Add,
                        WE.Err(VarNotDeclared)
                    )))
        ))),
        true
    ),
    (
        "((def o (new A ())) (o isa A))",
        WE.Node(Program(
            clss = List(),
            progb = WE.Node(ProgBlock(
                decls = List(
                    WE.Node(Decl(
                        WE.Node("o"), 
                        WE.Node(Expr.NewInstance(WE.Err(ClassNotDeclared), List()))
                    ))
                ),
                stmts = List(),
                expr = WE.Node(Expr.IsInstanceOf(
                    WE.Node("o"), 
                    WE.Err(ClassNotDeclared)
                ))))
        )),
        true
    )
    )

    val validSysCases = Seq(
    (
        """
        ((def guard 1.0) 
         (while0 guard 
                 (block (def i 0.0) 
                        (i = (i + guard)))) 
         guard)
        """,
        System[Clean](
            modules = List(),
            imports = List(),
            progb = ProgBlock(
                decls = List(
                    Decl("guard", Expr.Num(1.0))
                ),
                stmts = List(
                    Stmt.While(
                    guard = Expr.Var("guard"),
                    body = StmtBlock.Many(
                        decls = List(
                        Decl("i", Expr.Num(0.0))
                        ),
                        stmts = List(
                        Stmt.Assign(
                            "i",
                            Expr.BinOpExpr("i", BinOp.Add, "guard")
                        )
                        )
                    )
                    )
                ),
                expr = Expr.Var("guard")
            )
        ),
        false
    ),
    (
        """
        ((module OWO (class A ())) 
         (module UWU (import OWO)
          (class B (owo) 
            (method fave () 
               (def fave 413.0)
               (def o1 (new A ())) 
               (def o2 (new B (fave)))
               (this --> owo))))
         (import UWU)
         (import UWU)
         (def o (new B ())) 
         (o isa B))
        """,
        System[Clean](
            modules = List(
                Module(
                    "OWO",
                    List(),
                    Class("A", List(), List()),
                    None
                ),
                Module(
                    "UWU",
                    List("OWO"),
                    Class(
                        "B", 
                        List("owo"),
                        List(
                            Method(
                                "fave",
                                List(),
                                progb = ProgBlock(
                                    List(
                                        Decl("fave", Expr.Num(413.0)),
                                        Decl("o1", Expr.NewInstance("A", List())),
                                        Decl("o2", Expr.NewInstance("B", List("fave")))),
                                    List(),
                                    Expr.GetField("this", "owo")
                                )
                            )
                        )
                    )
                    ,
                    None
                )
            ),
            imports = List("UWU", "UWU"),
            progb = ProgBlock(
                decls = List(
                    Decl("o", Expr.NewInstance("B", List()))
                ),
                stmts = List(),
                expr = Expr.IsInstanceOf("o", "B"))
        ),
        false
    ),
    (
        """
        ((module OWO (class A ())) 
         (module UWU (import OWO)
          (class B (owo) 
            (method uwu (num) 
               (def foo 123.4) 
               (foo = num) 
               this) 
            (method fave () 
               (def o (new A ())) 
               413.0)))
         (import UWU)
         (import UWU)
         (def o (new B ())) 
         (o isa B))
        """,
        System[Clean](
            modules = List(
                Module(
                    "OWO",
                    List(),
                    Class("A", List(), List()),
                    None
                ),
                Module(
                    "UWU",
                    List("OWO"),
                    Class(
                        "B", 
                        List("owo"),
                        List(
                            Method(
                                "uwu",
                                List("num"),
                                progb = ProgBlock(
                                    List(Decl("foo", Expr.Num(123.4))), 
                                    List(Stmt.Assign("foo", Expr.Var("num"))),
                                    Expr.Var("this")
                                )
                            ),
                            Method(
                                "fave",
                                List(),
                                progb = ProgBlock(
                                    List(Decl("o", Expr.NewInstance("A", List()))),
                                    List(),
                                    Expr.Num(413.0)
                                )
                            )
                        )
                    )
                    ,
                    None
                )
            ),
            imports = List("UWU", "UWU"),
            progb = ProgBlock(
                decls = List(
                    Decl("o", Expr.NewInstance("B", List()))
                ),
                stmts = List(),
                expr = Expr.IsInstanceOf("o", "B"))
        ),
        false
    ),
    (
        """
        ((module WAOW (class B (owo) 
            (method uwu (num) 
                (def foo 123.4) 
                (foo = num) this)))
         (module WOAW (class B ()))
         (import WAOW)
         (import WOAW)
         (def num 413.0) 
         (def o (new B (num))) 
         (o --> notafield = (o --> somemethod (num))) 
         (o --> alsonotafield))
        """,
        System[Clean](
            modules = List(
                Module(
                    "WAOW",
                    List(),
                    Class(
                        "B", 
                        List("owo"),
                        List(
                            Method(
                                "uwu",
                                List("num"),
                                progb = ProgBlock(
                                    List(Decl("foo", Expr.Num(123.4))), 
                                    List(Stmt.Assign("foo", Expr.Var("num"))),
                                    Expr.Var("this"))
                            )
                        )
                    )
                    ,
                    None
                ),
                Module(
                    "WOAW",
                    List(),
                    Class("B", List(), List()),
                    None
                )
            ),
            imports = List("WAOW", "WOAW"),
            progb = ProgBlock(
                decls = List(
                    Decl("num", Expr.Num(413.0)),
                    Decl("o", Expr.NewInstance("B", List("num")))
                ),
                stmts = List(
                    Stmt.FieldAssign(
                        "o", 
                        "notafield", 
                        Expr.CallMethod("o", "somemethod", List("num")))
                ),
                expr = Expr.GetField("o", "alsonotafield"))
        ),
        false
    )  
    )

    val invalidSysCases = Seq(
    (
        """
        ((module A (class C ())) 
         (module A (class D ())) 
         413.0)
        """,
        WE.Node(System(
            modules = List(
                WE.Node(Module(WE.Node("A"), List(), WE.Node(Class(WE.Node("C"), List(), List())),None)),
                WE.Node(Module(WE.Err(DuplicateModuleName), List(), WE.Node(Class(WE.Node("D"), List(), List())),None))
            ),
            imports = List(),
            progb = WE.Node(ProgBlock(
                decls = List(),
                stmts = List(),
                expr = WE.Node(Expr.Num(413.0))
            ))
        )),
        true
    ),
    (
        """
        ((module A (class C ())) 
         (import C)
         413.0)
        """,
        WE.Node(System(
            modules = List(
                WE.Node(Module(WE.Node("A"), List(), WE.Node(Class(WE.Node("C"), List(), List())),None))
            ),
            imports = List(WE.Err(ModuleNotDeclared)),
            progb = WE.Node(ProgBlock(
                decls = List(),
                stmts = List(),
                expr = WE.Node(Expr.Num(413.0))
            ))
        )),
        true
    ),
    (
        """
        ((module A 
            (class A (A)
                (method A (A) A)))
         (module B 
            (class B (a a) 
                (method owo (a a) 612.0) 
                (method owo () -413.0))) 
         413.0)
        """,
        WE.Node(System(
            modules = List(
                WE.Node(Module(
                    WE.Node("A"), 
                    List(), 
                    WE.Node(Class(
                        WE.Node("A"), 
                        List(WE.Node("A")), 
                        List(
                            WE.Node(Method(
                                WE.Node("A"),
                                List(WE.Node("A")),
                                progb = WE.Node(ProgBlock(
                                    List(), 
                                    List(),
                                    WE.Node(Expr.Var(WE.Node("A")))))
                            ))
                        )))
                    ,
                    None
                )),
                WE.Node(Module(
                    WE.Node("B"), 
                    List(), 
                    WE.Node(Class(
                        WE.Node("B"), 
                        List(WE.Node("a"), WE.Err(DuplicateFieldName)), 
                        List(
                            WE.Node(Method(
                                WE.Node("owo"),
                                List(WE.Node("a"), WE.Err(DuplicateParamName)),
                                progb = WE.Node(ProgBlock(
                                    List(), 
                                    List(),
                                    WE.Node(Expr.Num(612.0))))
                            )),
                            WE.Node(Method(
                                WE.Err(DuplicateMethod),
                                List(),
                                progb = WE.Node(ProgBlock(
                                    List(), 
                                    List(),
                                    WE.Node(Expr.Num(-413.0)))
                            )))
                        )
                    ))
                    ,
                    None
                )),
            ),
            imports = List(),
            progb = WE.Node(ProgBlock(
                decls = List(),
                stmts = List(),
                expr = WE.Node(Expr.Num(413.0))))
        )),
        true
    ),
    (
        """
        ((module Amod (import Bmod)
            (class A (A)
                (method A (A) A))) 
         (module Bmod (import A)
            (class B (B)))
         (import Bmod) 
         (def x 413.0)
         (new A (x)))
        """,
        WE.Node(System(
            modules = List(
                WE.Node(Module(
                    WE.Node("Amod"), 
                    List(WE.Err(ModuleNotDeclared)), 
                    WE.Node(Class(
                        WE.Node("A"), 
                        List(WE.Node("A")), 
                        List(
                            WE.Node(Method(
                                WE.Node("A"),
                                List(WE.Node("A")),
                                progb = WE.Node(ProgBlock(
                                    List(), 
                                    List(),
                                    WE.Node(Expr.Var(WE.Node("A")))))
                            ))
                        )))
                    ,
                    None
                )),
                WE.Node(Module(
                    WE.Node("Bmod"), 
                    List(WE.Err(ModuleNotDeclared)), 
                    WE.Node(Class(
                        WE.Node("B"), 
                        List(WE.Node("B")), 
                        List()))
                    ,
                    None))
            ),
            imports = List(WE.Node("Bmod")),
            progb = WE.Node(ProgBlock(
                decls = List(WE.Node(Decl(WE.Node("x"), WE.Node(Expr.Num(413.0))))),
                stmts = List(),
                expr = WE.Node(Expr.NewInstance(WE.Err(ClassNotDeclared), List(WE.Node("x"))))
            ))
        )),
        true
    ),
    )
}