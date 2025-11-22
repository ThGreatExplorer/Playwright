package test

import munit.FunSuite
import ast._
import util.{UnreachableStateException, UnreachablePatternMatch}
import cesk.{CESKMachine, Store, Env, KontStack, ProgFrame, RuntimeError}
import cesk.CESKValue
import cesk.CESKConst

class CESKTests extends FunSuite {

  // Helper method to create a simple program with just an expression
  def simpleProgram(expr: CleanExpr): CleanProgram = 
    Program[Clean](clss = List(), progb = ProgBlock(decls = List(), stmts = List(), expr = expr))

  // Helper method to create a program with declarations
  def programWithDecls(decls: List[CleanDecl], expr: CleanExpr): CleanProgram =
    Program[Clean](clss = List(), progb = ProgBlock(decls = decls, stmts = List(), expr = expr))

  // Helper method to create a program with classes
  def programWithClasses(classes: List[CleanClass], decls: List[CleanDecl], expr: CleanExpr): CleanProgram =
    Program[Clean](clss = classes, progb = ProgBlock( decls = decls, stmts = List(), expr = expr))

  // Helper method to create a program with classes
  def fullProgram(classes: List[CleanClass], decls: List[CleanDecl], stmts : List[CleanStmt], expr: CleanExpr): CleanProgram =
    Program[Clean](clss = classes, progb = ProgBlock( decls = decls, stmts = stmts, expr = expr))

  test("Test malformed input triggers exception") {
    val cleanProgram = Program[Clean](
      clss = List(),
      progb = ProgBlock(
        decls = List(),
        stmts = List(),
        expr = Expr.BinOpExpr("foo", BinOp.Div, "foo"))
    )
    interceptMessage[UnreachablePatternMatch]("Should never happen: variable foo not found in environment") {
      CESKMachine(cleanProgram).run
    }
  }

  test("Test Store") {
    val store = Store()
    assertEquals(store.toString(), "Map()")
    interceptMessage[UnreachablePatternMatch]("Should never happen: location 1 not found in store") {
      store.getVal(1)
    }
    val (store2, loc) = store.insertValAtNewLoc(2.0)
    assertEquals(store2.toString(), f"Map($loc -> 2.0)")
    assertEquals(store2.getVal(loc), 2.0)
  }
  
  test("Test Env"){
    val env = Env()
    interceptMessage[UnreachablePatternMatch]("Should never happen: variable x not found in environment") {
      env.getLoc("x")
    }
    val env2 = env.updatedEnv("x", 1)
    assertEquals(env2.toString(), "Map(x -> 1)")
  }

  test("Test KontStack"){
    val kont = KontStack()
    assertEquals(kont.toString(), "List()")
    val kont2 = kont.push((ProgFrame(Nil, Nil, ()), Env()))
    assertEquals(kont2.toString(), "List((ProgFrame(List(),List(),()),Map()))")
    val kont3 = kont2.pop
    assertEquals(kont3.toString(), "List()")
  }

  // Test Case 1: Simple numeric literal
  test("Simple numeric literal returns the number") {
    val prog = simpleProgram(Expr.Num(42.0))
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 42.0)
  }

  // Test Case 2: Variable declaration and lookup
  test("Variable declaration and lookup") {
    val decl = Decl[Clean]("x", Expr.Num(10.0))
    val expr = Expr.Var[Clean]("x")
    val prog = programWithDecls(List(decl), expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 10.0)
  }

  // Test Case 3: Binary operations - Addition
  test("Binary addition of two variables") {
    val decls = List(
      Decl[Clean]("a", Expr.Num(5.0)),
      Decl[Clean]("b", Expr.Num(3.0))
    )
    val expr = Expr.BinOpExpr[Clean]("a", BinOp.Add, "b")
    val prog = programWithDecls(decls, expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 8.0)
  }

  // Test Case 4: Binary operations - Division
  test("Binary division of two variables") {
    val decls = List(
      Decl[Clean]("x", Expr.Num(15.0)),
      Decl[Clean]("y", Expr.Num(3.0))
    )
    val expr = Expr.BinOpExpr[Clean]("x", BinOp.Div, "y")
    val prog = programWithDecls(decls, expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 5.0)
  }

  // Test Case 5: Division by zero error
  test("Division by zero produces runtime error") {
    val decls = List(
      Decl[Clean]("x", Expr.Num(10.0)),
      Decl[Clean]("y", Expr.Num(0.0))
    )
    val expr = Expr.BinOpExpr[Clean]("x", BinOp.Div, "y")
    val prog = programWithDecls(decls, expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assert(result.isInstanceOf[RuntimeError.DivisionByZero])
  }

  // Test Case 6: Equality comparison - true case
  test("Equality comparison returns truthy for equal values") {
    val decls = List(
      Decl[Clean]("a", Expr.Num(7.0)),
      Decl[Clean]("b", Expr.Num(7.0))
    )
    val expr = Expr.BinOpExpr[Clean]("a", BinOp.Equals, "b")
    val prog = programWithDecls(decls, expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 0.0)
  }

  // Test Case 7: Equality comparison - false case
  test("Equality comparison returns falsy for unequal values") {
    val decls = List(
      Decl[Clean]("a", Expr.Num(5.0)),
      Decl[Clean]("b", Expr.Num(3.0))
    )
    val expr = Expr.BinOpExpr[Clean]("a", BinOp.Equals, "b")
    val prog = programWithDecls(decls, expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 1.0)
  }

  // Test Case 8: Equality Comparison among objects and objects vs Nums
  test("Equality Comparison among objects and objects vs Nums") {
    val pointClass = Class[Clean](
      cname = "Point",
      fields = List("x", "y"),
      methods = List(),
      shape = None
    )
    val knotClass = Class[Clean](
      cname = "Knot",
      fields = List("s"),
      methods = List(),
      shape = None
    )
    val dKnotClass = Class[Clean](
      cname = "DKnot",
      fields = List("r", "t"),
      methods = List(),
      shape = None
    )
    val decls = List(
      Decl[Clean]("px", Expr.Num(3.0)),
      Decl[Clean]("py", Expr.Num(4.0)),
      Decl[Clean]("pone", Expr.NewInstance("Point", List("px", "py"))),
      Decl[Clean]("ptwo", Expr.NewInstance("Point", List("px", "py"))),
      Decl[Clean]("knotSelfA", Expr.NewInstance("Knot", List("px"))),
      Decl[Clean]("knotSelfB", Expr.NewInstance("Knot", List("px"))),
      Decl[Clean]("knotSelfBB", Expr.NewInstance("Knot", List("knotSelfB"))),
      Decl[Clean]("knotBase", Expr.NewInstance("Knot", List("px"))),
      Decl[Clean]("knotOne",  Expr.NewInstance("Knot", List("knotBase"))),
      Decl[Clean]("knotTwo",  Expr.NewInstance("Knot", List("knotOne"))),
      Decl[Clean]("dknotA",  Expr.NewInstance("DKnot", List("knotOne", "knotBase"))),
      Decl[Clean]("dknotB",  Expr.NewInstance("DKnot", List("knotBase", "knotOne"))),
    )
    val stmts = List(
      Stmt.Assign[Clean]("px", Expr.Var("pone")),
      Stmt.FieldAssign[Clean]("knotSelfA", "s", Expr.Var("knotSelfA")),
      Stmt.FieldAssign[Clean]("knotSelfB", "s", Expr.Var("knotSelfB")),
    )

    def runAndAssertEq(lhs : String, rhs : String, expectedEq : Boolean): Unit =
      val prog =
        fullProgram(
          classes = List(pointClass, knotClass, dKnotClass),
          decls = decls, 
          stmts = stmts, 
          expr = Expr.BinOpExpr[Clean](lhs, BinOp.Equals, rhs)
        )
      val machine = CESKMachine(prog)
      val expectedResult = if expectedEq then CESKConst.TRUTHY else CESKConst.FALSY
      val result = machine.run
      assertEquals(result, expectedResult)

    // check object, num comparison returns false
    runAndAssertEq("pone", "py", false)
    runAndAssertEq("py", "pone", false)
    // check referential equality
    runAndAssertEq("pone", "pone", true)
    runAndAssertEq("px", "pone", true)
    // check structural equality
    runAndAssertEq("pone", "ptwo", true)
    runAndAssertEq("knotOne", "knotOne", true)
    runAndAssertEq("knotTwo", "knotOne", false)
    runAndAssertEq("knotTwo", "knotTwo", true)
    // check that self-loops are valid
    runAndAssertEq("knotSelfA", "knotSelfA", true)
    runAndAssertEq("knotSelfA", "knotSelfB", true)
    runAndAssertEq("knotSelfA", "knotSelfBB", true)
    runAndAssertEq("knotSelfB", "knotSelfB", true)
    // check structural equality with mutliple fields to recur over
    runAndAssertEq("dknotA", "dknotA", true)
    runAndAssertEq("dknotA", "dknotB", false)
    runAndAssertEq("dknotB", "dknotA", false)
    runAndAssertEq("dknotB", "dknotB", true)
  }

  // Test Case 8: BinOps Invalid Comparisons between Num and Obj
  test("BinOps Invalid Comparisons between Num and Obj") {
    val pointClass = Class[Clean](
      cname = "Point",
      fields = List("x", "y"),
      methods = List(),
      shape = None
    )
    val decls = List(
      Decl[Clean]("px", Expr.Num(3.0)),
      Decl[Clean]("py", Expr.Num(4.0)),
      Decl[Clean]("pone", Expr.NewInstance("Point", List("px", "py"))),
      Decl[Clean]("ptwo", Expr.NewInstance("Point", List("px", "py")))
    )

    val expr = Expr.BinOpExpr[Clean]("pone", BinOp.Add, "pone")

    def runAndAssertProg(expr : Expr[Clean], expectedResult : CESKValue | RuntimeError): Unit =
      val prog = programWithClasses(List(pointClass), decls, expr)
      val machine = CESKMachine(prog)
      val result = machine.run
      assertEquals(result, expectedResult)

    runAndAssertProg(expr, RuntimeError.InvalidVarType("Binop attempted on a non-numeric value."))
    val expr2 = Expr.BinOpExpr[Clean]("pone", BinOp.Add, "px")
    runAndAssertProg(expr2, RuntimeError.InvalidVarType("Binop attempted on a non-numeric value."))
    val expr3 = Expr.BinOpExpr[Clean]("px", BinOp.Add, "pone")
    runAndAssertProg(expr3, RuntimeError.InvalidVarType("Binop attempted on a non-numeric value."))
  }

  // Test Case 8: Object creation and field access
  test("Object creation, field access and no variable aliasing") {
    val pointClass = Class[Clean](
      cname = "Point",
      fields = List("x", "y"),
      methods = List(),
      shape = None
    )
    val decls = List(
      Decl[Clean]("px", Expr.Num(3.0)),
      Decl[Clean]("py", Expr.Num(4.0)),
      Decl[Clean]("p", Expr.NewInstance("Point", List("px", "py")))
    )
    val stmts = List(
      Stmt.Assign[Clean]("px", Expr.Num(300.0))
    )
    val expr = Expr.GetField[Clean]("p", "x")
    val prog = fullProgram(
        List(pointClass), decls, stmts, expr
      )
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 3.0)
  }

  test("Object creation with wrong number of Fields") {
    val pointClass = Class[Clean](
      cname = "Point",
      fields = List("x", "y"),
      methods = List(),
      shape = None
    )
    val decls = List(
      Decl[Clean]("px", Expr.Num(3.0)),
      Decl[Clean]("py", Expr.Num(4.0)),
      Decl[Clean]("p", Expr.NewInstance("Point", List("px")))
    )
    val expr = Expr.GetField[Clean]("p", "x")
    val prog = programWithClasses(List(pointClass), decls, expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, RuntimeError.NewInstWrongFieldCount)
  }

  test("Object creation and invalid field access") {
    val pointClass = Class[Clean](
      cname = "Point",
      fields = List("x", "y"),
      methods = List(),
      shape = None
    )
    val decls = List(
      Decl[Clean]("px", Expr.Num(3.0)),
      Decl[Clean]("py", Expr.Num(4.0)),
      Decl[Clean]("p", Expr.NewInstance("Point", List("px", "py")))
    )
    val expr = Expr.GetField[Clean]("p", "somefield")
    val prog = programWithClasses(List(pointClass), decls, expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, RuntimeError.FieldNotFound)
  }

  // Test Case 9: Object method call
  test("Object method call with parameters") {
    val addMethod = Method[Clean](
      mname = "add",
      params = List("val"),
      progb = ProgBlock(
        decls = List(
          Decl[Clean]("tmpBase", Expr.GetField[Clean]("this", "x"))
        ),
        stmts = List(),
        expr = Expr.BinOpExpr("tmpBase", BinOp.Add, "val"))
    )
    val numberClass = Class[Clean](
      cname = "Number",
      fields = List("x"),
      methods = List(addMethod),
      shape = None
    )
    val decls = List(
      Decl[Clean]("base", Expr.Num(10.0)),
      Decl[Clean]("num", Expr.NewInstance("Number", List("base"))),
      Decl[Clean]("increment", Expr.Num(5.0))
    )
    val expr = Expr.CallMethod[Clean]("num", "add", List("increment"))
    val prog = programWithClasses(List(numberClass), decls, expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 15.0)
  }

  test("Object method call with wrong number of parameters") {
    val addMethod = Method[Clean](
      mname = "add",
      params = List("val"),
      progb = ProgBlock(
        decls = List(
          Decl[Clean]("tmpBase", Expr.GetField[Clean]("this", "x"))
        ),
        stmts = List(),
        expr = Expr.BinOpExpr("tmpBase", BinOp.Add, "val"))
    )
    val numberClass = Class[Clean](
      cname = "Number",
      fields = List("x"),
      methods = List(addMethod),
      shape = None
    )
    val decls = List(
      Decl[Clean]("base", Expr.Num(10.0)),
      Decl[Clean]("num", Expr.NewInstance("Number", List("base"))),
      Decl[Clean]("increment", Expr.Num(5.0))
    )
    val expr = Expr.CallMethod[Clean]("num", "add", List("increment", "increment"))
    val prog = programWithClasses(List(numberClass), decls, expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, RuntimeError.MethodCallWrongArgCount)
  }
  

  // Test Case 10: IsInstanceOf check - positive case
  test("IsInstanceOf returns true for correct type") {
    val pointClass = Class[Clean](
      cname = "Point",
      fields = List("x"),
      methods = List(),
      shape = None
    )
    val decls = List(
      Decl[Clean]("val", Expr.Num(1.0)),
      Decl[Clean]("p", Expr.NewInstance("Point", List("val")))
    )
    val expr = Expr.IsInstanceOf[Clean]("p", "Point")
    val prog = programWithClasses(List(pointClass), decls, expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 0.0)
  }

  // Test Case 11: IsInstanceOf check - negative case
  test("IsInstanceOf returns false for incorrect type") {
    val pointClass = Class[Clean](
      cname = "Point",
      fields = List("x"),
      methods = List(),
      shape = None
    )
    val decls = List(
      Decl[Clean]("val", Expr.Num(1.0)),
      Decl[Clean]("p", Expr.NewInstance("Point", List("val"))),
      Decl[Clean]("lhs", Expr.IsInstanceOf[Clean]("p", "Circle")),
      Decl[Clean]("rhs", Expr.IsInstanceOf[Clean]("val", "Circle")),
    )
    val expr = Expr.BinOpExpr[Clean]("lhs", BinOp.Add, "rhs")
    val prog = programWithClasses(List(pointClass), decls, expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 2.0)
  }

  // Test Case 12: Assignment statement
  test("Variable assignment updates value") {
    val decls = List(Decl[Clean]("x", Expr.Num(5.0)))
    val stmts = List(Stmt.Assign[Clean]("x", Expr.Num(10.0)))
    val expr = Expr.Var[Clean]("x")
    val prog = Program[Clean](clss = List(), progb = ProgBlock(decls = decls, stmts = stmts, expr = expr))
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 10.0)
  }

  // Test Case 13: Field assignment
  test("Field assignment updates object field") {
    val pointClass = Class[Clean](
      cname = "Point",
      fields = List("x", "y"),
      methods = List(),
      shape = None
    )
    val decls = List(
      Decl[Clean]("px", Expr.Num(1.0)),
      Decl[Clean]("py", Expr.Num(2.0)),
      Decl[Clean]("p", Expr.NewInstance("Point", List("px", "py")))
    )
    val stmts = List(Stmt.FieldAssign[Clean]("p", "x", Expr.Num(99.0)))
    val expr = Expr.GetField[Clean]("p", "x")
    val prog = Program[Clean](clss = List(pointClass), progb = ProgBlock( decls = decls, stmts = stmts, expr = expr))
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 99.0)
  }
  
  test("Field assignment tries updates object field that does not exist") {
    val pointClass = Class[Clean](
      cname = "Point",
      fields = List("x", "y"),
      methods = List(),
      shape = None
    )
    val decls = List(
      Decl[Clean]("px", Expr.Num(1.0)),
      Decl[Clean]("py", Expr.Num(2.0)),
      Decl[Clean]("p", Expr.NewInstance("Point", List("px", "py")))
    )
    val stmts = List(Stmt.FieldAssign[Clean]("p", "somefield", Expr.Num(99.0)))
    val expr = Expr.GetField[Clean]("p", "x")
    val prog = Program[Clean](clss = List(pointClass), progb = ProgBlock( decls = decls, stmts = stmts, expr = expr))
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, RuntimeError.FieldNotFound)
  }

  // Test Case 14: If-else conditional - true branch
  test("If-else takes true branch when condition is truthy") {
    val decls = List(
      Decl[Clean]("condition", Expr.Num(1.0)),
      Decl[Clean]("result", Expr.Num(0.0))
    )
    val stmts = List(
      Stmt.Ifelse[Clean](
        guard = Expr.Var("condition"),
        tbranch = StmtBlock.One(Stmt.Assign("result", Expr.Num(100.0))),
        ebranch = StmtBlock.One(Stmt.Assign("result", Expr.Num(200.0)))
      )
    )
    val expr = Expr.Var[Clean]("result")
    val prog = Program[Clean](clss = List(), progb = ProgBlock( decls = decls, stmts = stmts, expr = expr))
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 200.0)
  }

  // Test Case 15: If-else conditional - false branch
  test("If-else takes false branch when condition is falsy") {
    val decls = List(
      Decl[Clean]("condition", Expr.Num(0.0)),
      Decl[Clean]("result", Expr.Num(0.0))
    )
    val stmts = List(
      Stmt.Ifelse[Clean](
        guard = Expr.Var("condition"),
        tbranch = StmtBlock.One(Stmt.Assign("result", Expr.Num(100.0))),
        ebranch = StmtBlock.One(Stmt.Assign("result", Expr.Num(200.0)))
      )
    )
    val expr = Expr.Var[Clean]("result")
    val prog = Program[Clean](clss = List(), progb = ProgBlock(decls = decls, stmts = stmts, expr = expr))
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 100.0)
  }

  test("If else has object at guard position") {
    val pointClass = Class[Clean](
      cname = "Point",
      fields = List("x", "y"),
      methods = List(),
      shape = None
    )
    val decls = List(
      Decl[Clean]("px", Expr.Num(1.0)),
      Decl[Clean]("py", Expr.Num(2.0)),
      Decl[Clean]("p", Expr.NewInstance("Point", List("px", "py")))
    )
    val stmts = List(
      Stmt.Ifelse[Clean](
        guard = Expr.Var("p"),
        tbranch = StmtBlock.One(Stmt.Assign("px", Expr.Num(100.0))),
        ebranch = StmtBlock.One(Stmt.Assign("px", Expr.Num(200.0)))
      )
    )
    val expr = Expr.Var[Clean]("px")
    val prog = Program[Clean](clss = List(pointClass), progb = ProgBlock( decls = decls, stmts = stmts, expr = expr))
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 200.0)
  }


  // Test Case 16: While loop
  test("While loop executes correctly") {
    val decls = List(
      Decl[Clean]("counter", Expr.Num(0.0)),
      Decl[Clean]("one", Expr.Num(1.0)),
      Decl[Clean]("limit", Expr.Num(3.0))
    )
    val stmts = List(
      Stmt.While[Clean](
        guard = Expr.Var("counter"),
        body = StmtBlock.One[Clean](Stmt.Assign("counter", 
          Expr.BinOpExpr[Clean]("one", BinOp.Add, "counter"))),
      )
    )
    val expr = Expr.Var[Clean]("limit")
    val prog = Program[Clean](clss = List(), progb = ProgBlock(decls = decls, stmts = stmts, expr = expr))
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 3.0)
  }

  test("While has object at guard position") {
    val pointClass = Class[Clean](
      cname = "Point",
      fields = List("x", "y"),
      methods = List(),
      shape = None
    )
    val decls = List(
      Decl[Clean]("px", Expr.Num(1.0)),
      Decl[Clean]("py", Expr.Num(2.0)),
      Decl[Clean]("p", Expr.NewInstance("Point", List("px", "py")))
    )
    val stmts = List(
      Stmt.While[Clean](
        guard = Expr.Var("p"),
        body = StmtBlock.One[Clean](Stmt.Assign("px", 
          Expr.BinOpExpr[Clean]("py", BinOp.Add, "py"))),
      )
    )
    val expr = Expr.Var[Clean]("px")
    val prog = Program[Clean](clss = List(pointClass), progb = ProgBlock( decls = decls, stmts = stmts, expr = expr))
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 1.0)
  }

  // Test Case 17: Error - accessing field on non-object
  test("Field access on numeric value produces error") {
    val decls = List(Decl[Clean]("x", Expr.Num(42.0)))
    val expr = Expr.GetField[Clean]("x", "someField")
    val prog = programWithDecls(decls, expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assert(result.isInstanceOf[RuntimeError.ValNotAnObject.type])
  }

  // Test Case 18: Error - method call on non-object
  test("Method call on numeric value produces error") {
    val decls = List(Decl[Clean]("x", Expr.Num(42.0)))
    val expr = Expr.CallMethod[Clean]("x", "someMethod", List())
    val prog = programWithDecls(decls, expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assert(result.isInstanceOf[RuntimeError.ValNotAnObject.type])
  }

  // Test Case 19: Complex program with multiple declarations and operations
  test("Complex program with multiple operations") {
    val decls = List(
      Decl[Clean]("a", Expr.Num(10.0)),
      Decl[Clean]("b", Expr.Num(5.0)),
      Decl[Clean]("sum", Expr.BinOpExpr("a", BinOp.Add, "b")),
      Decl[Clean]("product", Expr.BinOpExpr("sum", BinOp.Add, "a"))
    )
    val expr = Expr.Var[Clean]("product")
    val prog = programWithDecls(decls, expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 25.0) // (10 + 5) + 10 = 25
  }

  // Test Case 20: Method with this reference
  test("Method accessing this reference") {
    val getXMethod = Method[Clean](
      mname = "getX",
      params = List(),
      progb = ProgBlock(
        decls = List(),
        stmts = List(),
        expr = Expr.GetField("this", "x"))
    )
    val pointClass = Class[Clean](
      cname = "Point",
      fields = List("x", "y"),
      methods = List(getXMethod),
      shape = None
    )
    val decls = List(
      Decl[Clean]("px", Expr.Num(42.0)),
      Decl[Clean]("py", Expr.Num(24.0)),
      Decl[Clean]("p", Expr.NewInstance("Point", List("px", "py")))
    )
    val expr = Expr.CallMethod[Clean]("p", "getX", List())
    val prog = programWithClasses(List(pointClass), decls, expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 42.0)
  }

  // ----- Complex Tests which paseed validity cases -----
  val ans = List(100.0, 1.5, 10.0, 1.0, 1.0, 0.0, RuntimeError.MethodNotFound)
  ValidityTests.validProgCases.zip(ans).foreach(
    {case ((inputStr, prog, isInValid), ans) =>
      if !isInValid then
        test(s"Valid Parser Prog CESK Tests: $inputStr"){
            val machine = CESKMachine(prog)
            val result = machine.run
            assertEquals(result, ans)
        }
    }
  )
}