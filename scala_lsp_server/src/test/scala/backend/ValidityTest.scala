package test.backend

import munit.FunSuite
import main.BackendFuncs
import ast._
import test.backend.AstRangeDefaults.*
import ast.ValidityErrNodes._
import ast.ConverterToClean.progToClean
import static.Parser
import static.{VCheckTLDups, VCheckMFPNameDups, VCheckUndefined}
import static.ModuleData
import ast.ConverterToClean.{systemToClean, rawSystemToClean}
import ast.Expr.Num

class ValidityTest extends FunSuite {

    def progWEOrClean(prog : ProgramWE) : Either[ProgramWE, CleanProgram] =
        progToClean(prog) match 
            case None            => Left(prog)
            case Some(cleanProg) => Right(cleanProg)
    
    def rawSysWEOrClean(sys : RawSystemWE) : Either[RawSystemWE, CleanRawSystem] =
        rawSystemToClean(sys) match 
            case None           => Left(sys)
            case Some(cleanSys) => Right(cleanSys)

    def sysWEOrClean(sys : SystemWE) : Either[SystemWE, CleanSystem] =
        systemToClean(sys) match 
            case None            => Left(sys)
            case Some(cleanSys) => Right(cleanSys)

    def processProgTestCase(testTuple : (String, ProgramWE | CleanProgram, Boolean)) : Unit = testTuple match
        case (inputStr, expectedAST, isErrExpected) => 
            val progType = if isErrExpected then "Well-formed Invalid" else "Well-formed Valid"
            val testName = s"$progType Prog: validity pipeline for input: $inputStr"
            test(testName) {
                val inputSexp = BackendFuncs.readSexp(inputStr)

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
                        assertEquals(stripRanges(progWE), expectedAST)
                        assertEquals(true, isErrExpected)
                    case Right(cleanProg) => 
                        assertEquals(stripRanges(cleanProg), expectedAST)
                        assertEquals(false, isErrExpected)

            }

    def processSysTestCase(testTuple : (String, RawSystemWE | SystemWE | CleanSystem, Boolean)) : Unit = testTuple match
        case (inputStr, expectedAST, isErrExpected) => 
            val progType = if isErrExpected then "Well-formed Invalid" else "Well-formed Valid"
            val testName = s"$progType System: validity pipeline for input: $inputStr"
            test(testName){
                val inputSexp = BackendFuncs.readSexp(inputStr)

                val pipeRes = 
                    for 
                        parsedSys <- rawSysWEOrClean(Parser.parseMixedSys(inputSexp))
                        vCheck1   <- rawSysWEOrClean(VCheckTLDups.moduleDupsSys(parsedSys))
                        vCheck2   <- rawSysWEOrClean(VCheckMFPNameDups.mfpDupsSys(vCheck1))
                        annotated <- Right(ModuleData.processSystem(vCheck2))
                        validSys  <- sysWEOrClean(VCheckUndefined.closedSystem(annotated))
                    yield 
                        validSys

                val normalizedPipeRes = pipeRes match
                    case Left(sysWE) => Left(stripRanges(sysWE))
                    case Right(cleanSys) => Right(stripRanges(cleanSys))

                val normalizedExpected = expectedAST match
                    case we @ WE.Node(_: RawSystem[WE]) => stripRanges(we)
                    case we @ WE.Node(_: System[WE]) => stripRanges(we)
                    case we: WE.Err => we
                    case clean: CleanSystem => stripRanges(clean)

                (normalizedPipeRes, normalizedExpected) match 
                    case (Left(WE.Node(RawSystem(modsWE, impsWE, progWE, _))), WE.Node(RawSystem[WE](expectedMods, expectedImps, expectedProgB, _)))  => 
                        assertEquals(modsWE, expectedMods)
                        assertEquals(impsWE, expectedImps)
                        assertEquals(progWE, expectedProgB)
                        assertEquals(true, isErrExpected)
                    case (Left(WE.Node(System(modsWE, impsWE, progWE, _, _))), WE.Node(System[WE](expectedMods, expectedImps, expectedProgB, _, _)))  => 
                        assertEquals(modsWE, expectedMods)
                        assertEquals(impsWE, expectedImps)
                        assertEquals(progWE, expectedProgB)
                        assertEquals(true, isErrExpected)
                    case (Right(System(mods, imps, prog, _, _)), System[Clean](expectedMods, expectedImps, expectedProgB, _, _)) => 
                        assertEquals(mods, expectedMods)
                        assertEquals(imps, expectedImps)
                        assertEquals(prog, expectedProgB)
                        assertEquals(false, isErrExpected)
                    case _ => fail("Wrong result type")
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
                    Decl("foo", Expr.Num(123.4, DummyRange), DummyRange)
                ),
                stmts = List(
                    Stmt.Assign("foo", Expr.Num(100.0, DummyRange), DummyRange)
                ),
                expr = Expr.Var("foo", DummyRange), DummyRange)
        , DummyRange),
        false
    ),
    (
        "((def a 1.0) (def b 2.0) (a = (a + b)) (a / b))",
        Program[Clean](
            clss = List(),
            progb = ProgBlock(
                decls = List(
                    Decl("a", Expr.Num(1.0, DummyRange), DummyRange),
                    Decl("b", Expr.Num(2.0, DummyRange), DummyRange)
                ),
                stmts = List(
                    Stmt.Assign(
                    lhs = "a",
                    rhs = Expr.BinOpExpr("a", BinOp.Add, "b", DummyRange)
                    , DummyRange)
                ),
                expr = Expr.BinOpExpr("a", BinOp.Div, "b", DummyRange)
            , DummyRange)
        , DummyRange),
        false
    ),
    (
        "((def x 10.0) (if0 x (block (def y 2.0) (y = 5.0)) (block (def z 3.0) (z = 7.0))) x)",
        Program[Clean](
            clss = List(),
            progb = ProgBlock(
                decls = List(
                    Decl("x", Expr.Num(10.0, DummyRange), DummyRange)
                ),
                stmts = List(
                    Stmt.Ifelse(
                    guard = Expr.Var("x", DummyRange),
                    tbranch = StmtBlock.Many(
                        decls = List(
                        Decl("y", Expr.Num(2.0, DummyRange), DummyRange)
                        ),
                        stmts = List(
                        Stmt.Assign("y", Expr.Num(5.0, DummyRange), DummyRange)
                        )
                    , DummyRange),
                    ebranch = StmtBlock.Many(
                        decls = List(
                        Decl("z", Expr.Num(3.0, DummyRange), DummyRange)
                        ),
                        stmts = List(
                        Stmt.Assign("z", Expr.Num(7.0, DummyRange), DummyRange)
                        )
                    , DummyRange), DummyRange)
                ),
                expr = Expr.Var("x", DummyRange)
            , DummyRange)    
        , DummyRange),
        false
    ),
    (
        "((def guard 1.0) (while0 guard (block (def i 0.0) (i = (i + guard)))) guard)",
        Program[Clean](
            clss = List(),
            progb = ProgBlock(
                decls = List(
                    Decl("guard", Expr.Num(1.0, DummyRange), DummyRange)
                ),
                stmts = List(
                    Stmt.While(
                    guard = Expr.Var("guard", DummyRange),
                    body = StmtBlock.Many(
                        decls = List(
                        Decl("i", Expr.Num(0.0, DummyRange), DummyRange)
                        ),
                        stmts = List(
                        Stmt.Assign(
                            "i",
                            Expr.BinOpExpr("i", BinOp.Add, "guard", DummyRange)
                        , DummyRange)
                        )
                    , DummyRange)
                    , DummyRange)
                ),
                expr = Expr.Var("guard", DummyRange)
            , DummyRange)
        , DummyRange),
        false
    ),
    (
        "((def a 10.0) (def b 20.0) (a = (a + b)) (if0 a (block (def c 1.0) (c = (a / b))) (block (def d 2.0) (d = (b / a)))) (a == b))",
        Program[Clean](
            clss = List(),
            progb = ProgBlock(
                decls = List(
                    Decl("a", Expr.Num(10.0, DummyRange), DummyRange),
                    Decl("b", Expr.Num(20.0, DummyRange), DummyRange)
                ),
                stmts = List(
                    Stmt.Assign(
                    "a",
                    Expr.BinOpExpr("a", BinOp.Add,"b", DummyRange)
                    , DummyRange),
                    Stmt.Ifelse(
                    guard = Expr.Var("a", DummyRange),
                    tbranch = StmtBlock.Many(
                        decls = List(
                        Decl("c", Expr.Num(1.0, DummyRange), DummyRange)
                        ),
                        stmts = List(
                        Stmt.Assign(
                            "c",
                            Expr.BinOpExpr("a", BinOp.Div, "b", DummyRange)
                        , DummyRange)
                        )
                    , DummyRange),
                    ebranch = StmtBlock.Many(
                        decls = List(
                        Decl("d", Expr.Num(2.0, DummyRange), DummyRange)
                        ),
                        stmts = List(
                        Stmt.Assign(
                            "d",
                            Expr.BinOpExpr("b", BinOp.Div, "a", DummyRange)
                        , DummyRange)
                        )
                    , DummyRange)
                    , DummyRange)
                ),
                expr = Expr.BinOpExpr("a", BinOp.Equals, "b", DummyRange)
            , DummyRange) 
        , DummyRange),
        false
    ),
    (
        "((class A ()) (class B (owo) (method uwu (num) (def foo 123.4) (foo = num) this) (method fave () (def o (new A ())) 413.0)) (def o (new A ())) (o isa A))",
        Program[Clean](
            clss = List(
                Class("A", List(), List(), None, DummyRange),
                Class(
                    "B", 
                    List("owo"),
                    List(
                        Method(
                            "uwu",
                            List("num"),
                            progb = ProgBlock(
                                List(Decl("foo", Expr.Num(123.4, DummyRange), DummyRange)), 
                                List(Stmt.Assign("foo", Expr.Var("num", DummyRange), DummyRange)),
                                Expr.Var("this", DummyRange)
                            , DummyRange)
                        , DummyRange),
                        Method(
                            "fave",
                            List(),
                            progb = ProgBlock(
                                List(Decl("o", Expr.NewInstance("A", List(), DummyRange), DummyRange)),
                                List(),
                                Expr.Num(413.0, DummyRange)
                            , DummyRange)
                        , DummyRange)
                    ),
                    None
                , DummyRange)
            ),
            progb = ProgBlock(
                decls = List(
                    Decl("o", Expr.NewInstance("A", List(), DummyRange), DummyRange)
                ),
                stmts = List(),
                expr = Expr.IsInstanceOf("o", "A", DummyRange), DummyRange)
        , DummyRange),
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
                                List(Decl("foo", Expr.Num(123.4, DummyRange), DummyRange)), 
                                List(Stmt.Assign("foo", Expr.Var("num", DummyRange), DummyRange)),
                                Expr.Var("this", DummyRange), DummyRange)
                        , DummyRange)
                    ),
                    None
                , DummyRange)
            ),
            progb = ProgBlock(
                decls = List(
                    Decl("num", Expr.Num(413.0, DummyRange), DummyRange),
                    Decl("o", Expr.NewInstance("B", List("num"), DummyRange), DummyRange)
                ),
                stmts = List(
                    Stmt.FieldAssign(
                        "o", 
                        "notafield", 
                        Expr.CallMethod("o", "somemethod", List("num"), DummyRange), DummyRange)
                ),
                expr = Expr.GetField("o", "alsonotafield", DummyRange), DummyRange)
        , DummyRange),
        false
    )   
    )

    val invalidProgCases = Seq(
    (
        "((class A ()) (class A ()) 413.0)",
        WE.Node(Program(
            clss = List(
                WE.Node(Class(WE.Node("A"), List(), List(), None, DummyRange)),
                WE.Node(Class(WE.Err(DuplicateClassName), List(), List(), None, DummyRange))
            ),
            progb = WE.Node(ProgBlock(
                decls = List(),
                stmts = List(),
                expr = WE.Node(Expr.Num(413.0, DummyRange))
            , DummyRange))
        , DummyRange)),
        true
    ),
    (
        "((class A (a)) (class B (a a) (method owo (a a) 612.0) (method owo () -413.0)) 413.0)",
        WE.Node(Program(
            clss = List(
                WE.Node(Class(WE.Node("A"), List(WE.Node("a")), List(), None, DummyRange)),
                WE.Node(Class(WE.Node("B"), List(WE.Node("a"), WE.Err(DuplicateFieldName)), 
                List(
                    WE.Node(Method(
                        WE.Node("owo"),
                        List(WE.Node("a"), WE.Err(DuplicateParamName)),
                        progb = WE.Node(ProgBlock(
                            List(), 
                            List(),
                            WE.Node(Expr.Num(612.0, DummyRange)), DummyRange))
                    , DummyRange)),
                    WE.Node(Method(
                        WE.Err(DuplicateMethod),
                        List(),
                        progb = WE.Node(ProgBlock(
                            List(), 
                            List(),
                            WE.Node(Expr.Num(-413.0, DummyRange)), DummyRange)
                    ), DummyRange))
                ),
                None, DummyRange))
            ),
            progb = WE.Node(ProgBlock(
                decls = List(),
                stmts = List(),
                expr = WE.Node(Expr.Num(413.0, DummyRange)), DummyRange))
        , DummyRange)),
        true
    ),
    (
        "((class A (a)) (a + this))",
        WE.Node(Program(
            clss = List(
                WE.Node(Class(WE.Node("A"), List(WE.Node("a")), List(), None, DummyRange))
            ),
            progb = WE.Node(ProgBlock(
                decls = List(),
                stmts = List(),
                expr = 
                    WE.Node(Expr.BinOpExpr(
                        WE.Err(VarNotDeclared),
                        BinOp.Add,
                        WE.Err(VarNotDeclared)
                    , DummyRange)), DummyRange)
        ), DummyRange)),
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
                        WE.Node(Expr.NewInstance(WE.Err(ClassNotDeclared), List(), DummyRange))
                    , DummyRange))
                ),
                stmts = List(),
                expr = WE.Node(Expr.IsInstanceOf(
                    WE.Node("o"), 
                    WE.Err(ClassNotDeclared)
                , DummyRange)), DummyRange))
        , DummyRange)),
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
                    Decl("guard", Expr.Num(1.0, DummyRange), DummyRange)
                ),
                stmts = List(
                    Stmt.While(
                    guard = Expr.Var("guard", DummyRange),
                    body = StmtBlock.Many(
                        decls = List(
                        Decl("i", Expr.Num(0.0, DummyRange), DummyRange)
                        ),
                        stmts = List(
                        Stmt.Assign(
                            "i",
                            Expr.BinOpExpr("i", BinOp.Add, "guard", DummyRange)
                        , DummyRange)
                        )
                    , DummyRange)
                    , DummyRange)
                ),
                expr = Expr.Var("guard", DummyRange)
            , DummyRange), 
            ModuleData(Nil)
        , DummyRange),
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
        {
            val modules : List[CleanModule] = List(
                Module(
                    "OWO",
                    List(),
                    Class("A", List(), List(), None, DummyRange)
                , DummyRange),
                Module(
                    "UWU",
                    List(Import.Untyped("OWO", DummyRange)),
                    Class(
                        "B", 
                        List("owo"),
                        List(
                            Method(
                                "fave",
                                List(),
                                progb = ProgBlock(
                                    List(
                                        Decl("fave", Expr.Num(413.0, DummyRange), DummyRange),
                                        Decl("o1", Expr.NewInstance("A", List(), DummyRange), DummyRange),
                                        Decl("o2", Expr.NewInstance("B", List("fave"), DummyRange), DummyRange)),
                                    List(),
                                    Expr.GetField("this", "owo", DummyRange)
                                , DummyRange)
                            , DummyRange)
                        ),
                        None
                    , DummyRange)
                , DummyRange)
            )

            System[Clean](
                modules,
                List(Import.Untyped("UWU", DummyRange), Import.Untyped("UWU", DummyRange)),
                ProgBlock(
                    decls = List(
                        Decl("o", Expr.NewInstance("B", List(), DummyRange), DummyRange)
                    ),
                    stmts = List(),
                    expr = Expr.IsInstanceOf("o", "B", DummyRange), DummyRange), 
                ModuleData(modules)
            , DummyRange)
        },
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
        {
            val modules : List[CleanModule] = 
                List(
                    Module(
                        "OWO",
                        List(),
                        Class("A", List(), List(), None, DummyRange)
                    , DummyRange),
                    Module(
                        "UWU",
                        List(Import.Untyped("OWO", DummyRange)),
                        Class(
                            "B", 
                            List("owo"),
                            List(
                                Method(
                                    "uwu",
                                    List("num"),
                                    progb = ProgBlock(
                                        List(Decl("foo", Expr.Num(123.4, DummyRange), DummyRange)), 
                                        List(Stmt.Assign("foo", Expr.Var("num", DummyRange), DummyRange)),
                                        Expr.Var("this", DummyRange)
                                    , DummyRange)
                                , DummyRange),
                                Method(
                                    "fave",
                                    List(),
                                    progb = ProgBlock(
                                        List(Decl("o", Expr.NewInstance("A", List(), DummyRange), DummyRange)),
                                        List(),
                                        Expr.Num(413.0, DummyRange)
                                    , DummyRange)
                                , DummyRange)
                            ),
                            None
                        , DummyRange)
                    , DummyRange)
                )

            System[Clean](
                modules,
                List(Import.Untyped("UWU", DummyRange), Import.Untyped("UWU", DummyRange)),
                ProgBlock(
                    decls = List(
                        Decl("o", Expr.NewInstance("B", List(), DummyRange), DummyRange)
                    ),
                    stmts = List(),
                    expr = Expr.IsInstanceOf("o", "B", DummyRange), DummyRange),
                ModuleData(modules)
            , DummyRange)
        },
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
        {
            val modules : List[CleanModule] = 
                List(
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
                                        List(Decl("foo", Expr.Num(123.4, DummyRange), DummyRange)), 
                                        List(Stmt.Assign("foo", Expr.Var("num", DummyRange), DummyRange)),
                                        Expr.Var("this", DummyRange), DummyRange)
                                , DummyRange)
                            ),
                            None
                        , DummyRange)
                    , DummyRange),
                    Module(
                        "WOAW",
                        List(),
                        Class("B", List(), List(), None, DummyRange)
                    , DummyRange)
                )

            System[Clean](
                modules,
                List(Import.Untyped("WAOW", DummyRange), Import.Untyped("WOAW", DummyRange)),
                ProgBlock(
                    decls = List(
                        Decl("num", Expr.Num(413.0, DummyRange), DummyRange),
                        Decl("o", Expr.NewInstance("B", List("num"), DummyRange), DummyRange)
                    ),
                    stmts = List(
                        Stmt.FieldAssign(
                            "o", 
                            "notafield", 
                            Expr.CallMethod("o", "somemethod", List("num"), DummyRange), DummyRange)
                    ),
                    expr = Expr.GetField("o", "alsonotafield", DummyRange), DummyRange),
                ModuleData(modules)
            , DummyRange)
        },
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
        WE.Node(RawSystem(
            modules = List(
                WE.Node(Module(WE.Node("A"), List(), WE.Node(Class(WE.Node("C"), List(), List(), None, DummyRange)), DummyRange)),
                WE.Node(Module(WE.Err(DuplicateModuleName), List(), WE.Node(Class(WE.Node("D"), Nil, Nil, None, DummyRange)), DummyRange))
            ),
            imports = List(),
            progb = WE.Node(ProgBlock(
                decls = List(),
                stmts = List(),
                expr = WE.Node(Expr.Num(413.0, DummyRange))
            , DummyRange))
        , DummyRange)),
        true
    ),
    (
        """
        ((module A (class C ())) 
         (import C)
         413.0)
        """,
        {
            WE.Node(System(
                List(
                    WE.Node(Module(WE.Node("A"), List(), WE.Node(Class(WE.Node("C"), List(), List(), None, DummyRange)), DummyRange))
                ),
                List(WE.Err(ModuleNotDeclared)),
                WE.Node(ProgBlock(
                    decls = List(),
                    stmts = List(),
                    expr = WE.Node(Expr.Num(413.0, DummyRange))
                , DummyRange)),
                ModuleData(List(
                    Module[Clean]("A", List(), Class("C", List(), List(), None, DummyRange), DummyRange)
                ))
            , DummyRange))
        },
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
        WE.Node(RawSystem(
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
                                    WE.Node(Expr.Var(WE.Node("A"), DummyRange)), DummyRange))
                            , DummyRange))
                        ),
                        None, DummyRange))
                , DummyRange)),
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
                                    WE.Node(Expr.Num(612.0, DummyRange)), DummyRange))
                            , DummyRange)),
                            WE.Node(Method(
                                WE.Err(DuplicateMethod),
                                List(),
                                progb = WE.Node(ProgBlock(
                                    List(), 
                                    List(),
                                    WE.Node(Expr.Num(-413.0, DummyRange)), DummyRange)
                            ), DummyRange))
                        ),
                        None
                    , DummyRange))
                , DummyRange)),
            ),
            imports = List(),
            progb = WE.Node(ProgBlock(
                decls = List(),
                stmts = List(),
                expr = WE.Node(Expr.Num(413.0, DummyRange)), DummyRange))
        , DummyRange)),
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
            List(
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
                                    WE.Node(Expr.Var(WE.Node("A"), DummyRange)), DummyRange))
                            , DummyRange))
                        ),
                        None, DummyRange))
                , DummyRange)),
                WE.Node(Module(
                    WE.Node("Bmod"), 
                    List(WE.Err(ModuleNotDeclared)), 
                    WE.Node(Class(
                        WE.Node("B"), 
                        List(WE.Node("B")), 
                        List(),
                        None, DummyRange)), DummyRange))
            ),
            List(WE.Node(Import.Untyped(WE.Node("Bmod"), DummyRange))),
            WE.Node(ProgBlock(
                decls = List(WE.Node(Decl(WE.Node("x"), WE.Node(Expr.Num(413.0, DummyRange)), DummyRange))),
                stmts = List(),
                expr = WE.Node(Expr.NewInstance(WE.Err(ClassNotDeclared), List(WE.Node("x")), DummyRange))
            , DummyRange)),
            ModuleData(List(
                Module[Clean](
                    "Amod",
                    List(Import.Untyped("Bmod", DummyRange)), 
                    Class("A", List("A"), List(Method("A", List("A"), ProgBlock(List(), List(), Expr.Var("A", DummyRange), DummyRange), DummyRange)), None, DummyRange), DummyRange),
                Module[Clean](
                    "Bmod",
                    List(Import.Untyped("A", DummyRange)), 
                    Class("B", List("B"), List(), None, DummyRange), DummyRange),
            ))
        , DummyRange)),
        true
    ),
    )
}
