package test

import munit.FunSuite
import main.MainFuncs
import static.Parser
import ast._
import ast.ValidityErrNodes._
import ast.ConverterToClean.progToClean
import static.{VCheckClassDups, VCheckMethodFieldParamDups, VCheckUndefined}

class ValidityTest extends FunSuite {

    def WEOrClean(prog : ProgramWE) : Either[ProgramWE, CleanProgram] =
        progToClean(prog) match 
            case None            => Left(prog)
            case Some(cleanProg) => Right(cleanProg)

    def processTestCase(testTuple : (String, ProgramWE | CleanProgram, Boolean)) : Unit = testTuple match
        case (inputStr, expectedAST, isErrExpected) => 
            test(s"Well-formed Parser Prog Invalid test for input: $inputStr") {
                val inputSexp = MainFuncs.readSexp(inputStr)

                val pipeRes = 
                    for 
                        parsedProg <- WEOrClean(Parser.parseProg(inputSexp))
                        vCheck1 <- WEOrClean(VCheckClassDups.classDupsProg(parsedProg))
                        vCheck2 <- WEOrClean(VCheckMethodFieldParamDups.mfpDupsProg(vCheck1))
                        validPr <- WEOrClean(VCheckUndefined.closedProg(vCheck2))
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


    ValidityTests.validCases.foreach(processTestCase)
    ValidityTests.invalidCases.foreach(processTestCase)
}

object ValidityTests {
    val validCases = Seq(
    (
        "((def foo 123.4) (foo = 100.0) foo)",
        Program[Clean](
            clss = List(),
            decls = List(
                Decl(Name("foo"), Expr.Num(123.4))
            ),
            stmts = List(
                Stmt.Assign(VarRef("foo"), Expr.Num(100.0))
            ),
            expr = Expr.Var(VarRef("foo"))
        ),
        false
    ),
    (
        "((def a 1.0) (def b 2.0) (a = (a + b)) (a / b))",
        Program[Clean](
            clss = List(),
            decls = List(
                Decl(Name("a"), Expr.Num(1.0)),
                Decl(Name("b"), Expr.Num(2.0))
            ),
            stmts = List(
                Stmt.Assign(
                lhs = VarRef("a"),
                rhs = Expr.BinOpExpr(VarRef("a"), BinOp.Add, VarRef("b"))
                )
            ),
            expr = Expr.BinOpExpr(VarRef("a"), BinOp.Div, VarRef("b"))
        ),
        false
    ),
    (
        "((def x 10.0) (if0 x (block (def y 2.0) (y = 5.0)) (block (def z 3.0) (z = 7.0))) x)",
        Program[Clean](
            clss = List(),
            decls = List(
                Decl(Name("x"), Expr.Num(10.0))
            ),
            stmts = List(
                Stmt.Ifelse(
                guard = Expr.Var(VarRef("x")),
                tbranch = Block.Many(
                    decls = List(
                    Decl(Name("y"), Expr.Num(2.0))
                    ),
                    stmts = List(
                    Stmt.Assign(VarRef("y"), Expr.Num(5.0))
                    )
                ),
                ebranch = Block.Many(
                    decls = List(
                    Decl(Name("z"), Expr.Num(3.0))
                    ),
                    stmts = List(
                    Stmt.Assign(VarRef("z"), Expr.Num(7.0))
                    )
                )
                )
            ),
            expr = Expr.Var(VarRef("x"))
        ),
        false
    ),
    (
        "((def guard 1.0) (while0 guard (block (def i 0.0) (i = (i + guard)))) guard)",
        Program[Clean](
            clss = List(),
            decls = List(
                Decl(Name("guard"), Expr.Num(1.0))
            ),
            stmts = List(
                Stmt.While(
                guard = Expr.Var(VarRef("guard")),
                body = Block.Many(
                    decls = List(
                    Decl(Name("i"), Expr.Num(0.0))
                    ),
                    stmts = List(
                    Stmt.Assign(
                        VarRef("i"),
                        Expr.BinOpExpr(VarRef("i"), BinOp.Add, VarRef("guard"))
                    )
                    )
                )
                )
            ),
            expr = Expr.Var(VarRef("guard"))
        ),
        false
    ),
    (
        "((def a 10.0) (def b 20.0) (a = (a + b)) (if0 a (block (def c 1.0) (c = (a / b))) (block (def d 2.0) (d = (b / a)))) (a == b))",
        Program[Clean](
            clss = List(),
            decls = List(
                Decl(Name("a"), Expr.Num(10.0)),
                Decl(Name("b"), Expr.Num(20.0))
            ),
            stmts = List(
                Stmt.Assign(
                VarRef("a"),
                Expr.BinOpExpr(VarRef("a"), BinOp.Add,VarRef("b"))
                ),
                Stmt.Ifelse(
                guard = Expr.Var(VarRef("a")),
                tbranch = Block.Many(
                    decls = List(
                    Decl(Name("c"), Expr.Num(1.0))
                    ),
                    stmts = List(
                    Stmt.Assign(
                        VarRef("c"),
                        Expr.BinOpExpr(VarRef("a"), BinOp.Div, VarRef("b"))
                    )
                    )
                ),
                ebranch = Block.Many(
                    decls = List(
                    Decl(Name("d"), Expr.Num(2.0))
                    ),
                    stmts = List(
                    Stmt.Assign(
                        VarRef("d"),
                        Expr.BinOpExpr(VarRef("b"), BinOp.Div, VarRef("a"))
                    )
                    )
                )
                )
            ),
            expr = Expr.BinOpExpr(VarRef("a"), BinOp.Equals, VarRef("b"))
        ),
        false
    ),
    (
        "((class A ()) (class B (owo) (method uwu (num) (def foo 123.4) (foo = num) this) (method fave () (def o (new A ())) 413.0)) (def o (new A ())) (o isa A))",
        Program[Clean](
            clss = List(
                Class(Name("A"), List(), List()),
                Class(
                    Name("B"), 
                    List(Name("owo")),
                    List(
                        Method(
                            Name("uwu"),
                            List(Name("num")),
                            List(Decl(Name("foo"), Expr.Num(123.4))), 
                            List(Stmt.Assign(VarRef("foo"), Expr.Var(VarRef("num")))),
                            Expr.Var(VarRef("this"))
                        ),
                        Method(
                            Name("fave"),
                            List(),
                            List(Decl(Name("o"), Expr.NewInstance(Name("A"), List()))),
                            List(),
                            Expr.Num(413.0)
                        )
                    )
                )
            ),
            decls = List(
                Decl(Name("o"), Expr.NewInstance(Name("A"), List()))
            ),
            stmts = List(),
            expr = Expr.IsInstanceOf(VarRef("o"), Name("A"))
        ),
        false
    ),
    (
        "((class B (owo) (method uwu (num) (def foo 123.4) (foo = num) this)) (def num 413.0) (def o (new B (num))) (o --> notafield = (o --> somemethod (num))) (o --> alsonotafield))",
        Program[Clean](
            clss = List(
                Class(
                    Name("B"), 
                    List(Name("owo")),
                    List(
                        Method(
                            Name("uwu"),
                            List(Name("num")),
                            List(Decl(Name("foo"), Expr.Num(123.4))), 
                            List(Stmt.Assign(VarRef("foo"), Expr.Var(VarRef("num")))),
                            Expr.Var(VarRef("this"))
                        )
                    )
                )
            ),
            decls = List(
                Decl(Name("num"), Expr.Num(413.0)),
                Decl(Name("o"), Expr.NewInstance(Name("B"), List(VarRef("num"))))
            ),
            stmts = List(
                Stmt.FieldAssign(
                    VarRef("o"), 
                    Name("notafield"), 
                    Expr.CallMethod(VarRef("o"), Name("somemethod"), List(VarRef("num"))))
            ),
            expr = Expr.GetField(VarRef("o"), Name("alsonotafield"))
        ),
        false
    )   
    )

    val invalidCases = Seq(
    (
        "((class A ()) (class A ()) 413.0)",
        WE.Node(Program(
            clss = List(
                WE.Node(Class(WE.Node(Name("A")), List(), List())),
                WE.Node(Class(WE.Err(DuplicateClassName), List(), List()))
            ),
            decls = List(),
            stmts = List(),
            expr = WE.Node(Expr.Num(413.0))
        )),
        true
    ),
    (
        "((class A (a)) (class B (a a) (method owo (a a) 612.0) (method owo () -413.0)) 413.0)",
        WE.Node(Program(
            clss = List(
                WE.Node(Class(WE.Node(Name("A")), List(WE.Node(Name("a"))), List())),
                WE.Node(Class(WE.Node(Name("B")), List(WE.Node(Name("a")), WE.Err(DuplicateFieldName)), 
                List(
                    WE.Node(Method(
                        WE.Node(Name("owo")),
                        List(WE.Node(Name("a")), WE.Err(DuplicateParamName)),
                        List(), 
                        List(),
                        WE.Node(Expr.Num(612.0))
                    )),
                    WE.Node(Method(
                        WE.Err(DuplicateMethod),
                        List(),
                        List(), 
                        List(),
                        WE.Node(Expr.Num(-413.0))
                    ))
                )))
            ),
            decls = List(),
            stmts = List(),
            expr = WE.Node(Expr.Num(413.0))
        )),
        true
    ),
    (
        "((class A (a)) (a + this))",
        WE.Node(Program(
            clss = List(
                WE.Node(Class(WE.Node(Name("A")), List(WE.Node(Name("a"))), List()))
            ),
            decls = List(),
            stmts = List(),
            expr = 
                WE.Node(Expr.BinOpExpr(
                    WE.Err(VarNotDeclared),
                    BinOp.Add,
                    WE.Err(VarNotDeclared)
                ))
        )),
        true
    ),
    (
        "((def o (new A ())) (o isa A))",
        WE.Node(Program(
            clss = List(),
            decls = List(
                WE.Node(Decl(
                    WE.Node(Name("o")), 
                    WE.Node(Expr.NewInstance(WE.Err(ClassNotDeclared), List()))
                ))
            ),
            stmts = List(),
            expr = WE.Node(Expr.IsInstanceOf(
                WE.Node(VarRef("o")), 
                WE.Err(ClassNotDeclared)
            ))
        )),
        true
    )
    )

}