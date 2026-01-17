package test.backend

import munit.FunSuite
import ast._
import test.backend.AstRangeDefaults.DummyRange
import util.{UnreachableStateException, UnreachablePatternMatch}
import cesk.{CESKMachine, Store, Env, KontStack, ProgFrame, RuntimeError}
import cesk.{CESKValue, ProxyVal, ObjectVal}
import scala.collection.mutable.Map as MutableMap
import cesk.CESKConst

class CESKTests extends FunSuite {

  // Helper method to create a simple program with just an expression
  def simpleProgram(expr: CleanExpr): CleanProgram =
    Program[Clean](
      clss = List(),
      progb = ProgBlock(decls = List(), stmts = List(), expr = expr, range = DummyRange),
      range = DummyRange
    )

  // Helper method to create a program with declarations
  def programWithDecls(decls: List[CleanDecl], expr: CleanExpr): CleanProgram =
    Program[Clean](
      clss = List(),
      progb = ProgBlock(decls = decls, stmts = List(), expr = expr, range = DummyRange),
      range = DummyRange
    )

  // Helper method to create a program with classes
  def programWithClasses(classes: List[CleanClass], decls: List[CleanDecl], expr: CleanExpr): CleanProgram =
    Program[Clean](
      clss = classes,
      progb = ProgBlock(decls = decls, stmts = List(), expr = expr, range = DummyRange),
      range = DummyRange
    )

  // Helper method to create a program with classes
  def fullProgram(classes: List[CleanClass], decls: List[CleanDecl], stmts : List[CleanStmt], expr: CleanExpr): CleanProgram =
    Program[Clean](
      clss = classes,
      progb = ProgBlock(decls = decls, stmts = stmts, expr = expr, range = DummyRange),
      range = DummyRange
    )

  test("Test malformed input triggers exception") {
    val cleanProgram = Program[Clean](
      clss = List(),
      progb = ProgBlock(
        decls = List(),
        stmts = List(),
        expr = Expr.BinOpExpr("foo", BinOp.Div, "foo", DummyRange),
        range = DummyRange
      ),
      range = DummyRange
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
    val prog = simpleProgram(Expr.Num(42.0, DummyRange))
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 42.0)
  }

  // Test Case 2: Variable declaration and lookup
  test("Variable declaration and lookup") {
    val decl = Decl[Clean]("x", Expr.Num(10.0, DummyRange), DummyRange)
    val expr = Expr.Var[Clean]("x", DummyRange)
    val prog = programWithDecls(List(decl), expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 10.0)
  }

  // Test Case 3: Binary operations - Addition
  test("Binary addition of two variables") {
    val decls = List(
      Decl[Clean]("a", Expr.Num(5.0, DummyRange), DummyRange),
      Decl[Clean]("b", Expr.Num(3.0, DummyRange), DummyRange)
    )
    val expr = Expr.BinOpExpr[Clean]("a", BinOp.Add, "b", DummyRange)
    val prog = programWithDecls(decls, expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 8.0)
  }

  // Test Case 4: Binary operations - Division
  test("Binary division of two variables") {
    val decls = List(
      Decl[Clean]("x", Expr.Num(15.0, DummyRange), DummyRange),
      Decl[Clean]("y", Expr.Num(3.0, DummyRange), DummyRange)
    )
    val expr = Expr.BinOpExpr[Clean]("x", BinOp.Div, "y", DummyRange)
    val prog = programWithDecls(decls, expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 5.0)
  }

  // Test Case 5: Division by zero error
  test("Division by zero produces runtime error") {
    val decls = List(
      Decl[Clean]("x", Expr.Num(10.0, DummyRange), DummyRange),
      Decl[Clean]("y", Expr.Num(0.0, DummyRange), DummyRange)
    )
    val expr = Expr.BinOpExpr[Clean]("x", BinOp.Div, "y", DummyRange)
    val prog = programWithDecls(decls, expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assert(result.isInstanceOf[RuntimeError.DivisionByZero])
  }

  // Test Case 6: Equality comparison - true case
  test("Equality comparison returns truthy for equal values") {
    val decls = List(
      Decl[Clean]("a", Expr.Num(7.0, DummyRange), DummyRange),
      Decl[Clean]("b", Expr.Num(7.0, DummyRange), DummyRange)
    )
    val expr = Expr.BinOpExpr[Clean]("a", BinOp.Equals, "b", DummyRange)
    val prog = programWithDecls(decls, expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 0.0)
  }

  // Test Case 7: Equality comparison - false case
  test("Equality comparison returns falsy for unequal values") {
    val decls = List(
      Decl[Clean]("a", Expr.Num(5.0, DummyRange), DummyRange),
      Decl[Clean]("b", Expr.Num(3.0, DummyRange), DummyRange)
    )
    val expr = Expr.BinOpExpr[Clean]("a", BinOp.Equals, "b", DummyRange)
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
      shape = None,
      range = DummyRange
    )
    val knotClass = Class[Clean](
      cname = "Knot",
      fields = List("s"),
      methods = List(),
      shape = None,
      range = DummyRange
    )
    val dKnotClass = Class[Clean](
      cname = "DKnot",
      fields = List("r", "t"),
      methods = List(),
      shape = None,
      range = DummyRange
    )
    val decls = List(
      Decl[Clean]("px", Expr.Num(3.0, DummyRange), DummyRange),
      Decl[Clean]("py", Expr.Num(4.0, DummyRange), DummyRange),
      Decl[Clean]("pone", Expr.NewInstance("Point", List("px", "py"), DummyRange), DummyRange),
      Decl[Clean]("ptwo", Expr.NewInstance("Point", List("px", "py"), DummyRange), DummyRange),
      Decl[Clean]("knotSelfA", Expr.NewInstance("Knot", List("px"), DummyRange), DummyRange),
      Decl[Clean]("knotSelfB", Expr.NewInstance("Knot", List("px"), DummyRange), DummyRange),
      Decl[Clean]("knotSelfBB", Expr.NewInstance("Knot", List("knotSelfB"), DummyRange), DummyRange),
      Decl[Clean]("knotBase", Expr.NewInstance("Knot", List("px"), DummyRange), DummyRange),
      Decl[Clean]("knotOne",  Expr.NewInstance("Knot", List("knotBase"), DummyRange), DummyRange),
      Decl[Clean]("knotTwo",  Expr.NewInstance("Knot", List("knotOne"), DummyRange), DummyRange),
      Decl[Clean]("dknotA",  Expr.NewInstance("DKnot", List("knotOne", "knotBase"), DummyRange), DummyRange),
      Decl[Clean]("dknotB",  Expr.NewInstance("DKnot", List("knotBase", "knotOne"), DummyRange), DummyRange),
    )
    val stmts = List(
      Stmt.Assign[Clean]("px", Expr.Var("pone", DummyRange), DummyRange),
      Stmt.FieldAssign[Clean]("knotSelfA", "s", Expr.Var("knotSelfA", DummyRange), DummyRange),
      Stmt.FieldAssign[Clean]("knotSelfB", "s", Expr.Var("knotSelfB", DummyRange), DummyRange),
    )

    def runAndAssertEq(lhs : String, rhs : String, expectedEq : Boolean): Unit =
      val prog =
        fullProgram(
          classes = List(pointClass, knotClass, dKnotClass),
          decls = decls, 
          stmts = stmts, 
          expr = Expr.BinOpExpr[Clean](lhs, BinOp.Equals, rhs, DummyRange)
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
      shape = Some(Type.Shape[Clean](
        List(
          FieldType("x", Type.Number(DummyRange), DummyRange),
          FieldType("y", Type.Number(DummyRange), DummyRange),
        ),
        List(),
        DummyRange
      )),
      range = DummyRange
    )
    val pointClassUntyped = Class[Clean](
      cname = "PointUntyped",
      fields = List("x", "y"),
      methods = List(),
      shape = None,
      range = DummyRange
    )
    val decls = List(
      Decl[Clean]("px", Expr.Num(3.0, DummyRange), DummyRange),
      Decl[Clean]("py", Expr.Num(4.0, DummyRange), DummyRange),
      Decl[Clean]("pone", Expr.NewInstance("Point", List("px", "py"), DummyRange), DummyRange),
      Decl[Clean]("ptwo", Expr.NewInstance("PointUntyped", List("px", "py"), DummyRange), DummyRange)
    )

    def runAndAssertBinopErrorProg(expr : Expr[Clean]): Unit =
      val prog = programWithClasses(List(pointClass, pointClassUntyped), decls, expr)
      val machine = CESKMachine(prog)
      val result = machine.run
      assertEquals(result, RuntimeError.InvalidVarType("Binop attempted on a non-numeric value."))

    runAndAssertBinopErrorProg(Expr.BinOpExpr[Clean]("pone", BinOp.Add, "ptwo", DummyRange))
    runAndAssertBinopErrorProg(Expr.BinOpExpr[Clean]("pone", BinOp.Add, "px", DummyRange))
    runAndAssertBinopErrorProg(Expr.BinOpExpr[Clean]("px", BinOp.Add, "pone", DummyRange))
    runAndAssertBinopErrorProg(Expr.BinOpExpr[Clean]("ptwo", BinOp.Div, "pone", DummyRange))
    runAndAssertBinopErrorProg(Expr.BinOpExpr[Clean]("pone", BinOp.Div, "px", DummyRange))
    runAndAssertBinopErrorProg(Expr.BinOpExpr[Clean]("px", BinOp.Div, "pone", DummyRange))
  }

  // Test Case 8: Object creation and field access
  test("Object creation, field access and no variable aliasing") {
    val pointClass = Class[Clean](
      cname = "Point",
      fields = List("x", "y"),
      methods = List(),
      shape = None,
      range = DummyRange
    )
    val decls = List(
      Decl[Clean]("px", Expr.Num(3.0, DummyRange), DummyRange),
      Decl[Clean]("py", Expr.Num(4.0, DummyRange), DummyRange),
      Decl[Clean]("p", Expr.NewInstance("Point", List("px", "py"), DummyRange), DummyRange)
    )
    val stmts = List(
      Stmt.Assign[Clean]("px", Expr.Num(300.0, DummyRange), DummyRange)
    )
    val expr = Expr.GetField[Clean]("p", "x", DummyRange)
    val prog = fullProgram(
        List(pointClass), decls, stmts, expr
      )
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 3.0)
  }

  // Proxy Test -- Test Field Access with Typed Classes
  test("Test Field Access with Typed Classes") {
    val pointClass = Class[Clean](
      cname = "Point",
      fields = List("x", "y"),
      methods = List(),
      shape = Some(Type.Shape[Clean](
        List(
          FieldType("x", Type.Number(DummyRange), DummyRange),
          FieldType("y", Type.Number(DummyRange), DummyRange),
        ),
        List(),
        DummyRange
      )),
      range = DummyRange
    )
    val decls = List(
      Decl[Clean]("px", Expr.Num(3.0, DummyRange), DummyRange),
      Decl[Clean]("py", Expr.Num(4.0, DummyRange), DummyRange),
      Decl[Clean]("p", Expr.NewInstance("Point", List("px", "py"), DummyRange), DummyRange)
    )
    val stmts = List(
      Stmt.Assign[Clean]("px", Expr.Num(300.0, DummyRange), DummyRange)
    )
    val expr = Expr.GetField[Clean]("p", "x", DummyRange)
    val prog = fullProgram(
        List(pointClass), decls, stmts, expr
      )
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 3.0)
  }

  // Structural Equality of Proxies
  test("Structural equality of Proxy Instances") {
    val pointClass = Class[Clean](
      cname = "Point",
      fields = List("x", "y"),
      methods = List(),
      shape = Some(Type.Shape[Clean](
        List(
          FieldType("x", Type.Number(DummyRange), DummyRange),
          FieldType("y", Type.Number(DummyRange), DummyRange),
        ),
        List(),
        DummyRange
      )),
      range = DummyRange
    )
    val pointClassUntyped = Class[Clean](
      cname = "PointUntyped",
      fields = List("x", "y"),
      methods = List(),
      shape = None,
      range = DummyRange
    )
    val decls = List(
      Decl[Clean]("px", Expr.Num(3.0, DummyRange), DummyRange),
      Decl[Clean]("py", Expr.Num(4.0, DummyRange), DummyRange),
      Decl[Clean]("p1", Expr.NewInstance("Point", List("px", "py"), DummyRange), DummyRange),
      Decl[Clean]("p2", Expr.NewInstance("Point", List("py", "px"), DummyRange), DummyRange),
      Decl[Clean]("p3", Expr.NewInstance("PointUntyped", List("py", "px"), DummyRange), DummyRange),
      Decl[Clean]("res1", Expr.BinOpExpr[Clean]("p1", BinOp.Equals, "p2", DummyRange), DummyRange),
      Decl[Clean]("res2", Expr.BinOpExpr[Clean]("p1", BinOp.Equals, "p3", DummyRange), DummyRange)
    )
    val prog =
      fullProgram(
        classes = List(pointClass, pointClassUntyped),
        decls = decls, 
        stmts = List(), 
        expr = Expr.BinOpExpr[Clean]("res1", BinOp.Equals, "res2", DummyRange)
      )
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, CESKConst.TRUTHY)
  }


  test("Test Field Access with Typed Classes Violation") {
    val pointClass = Class[Clean](
      cname = "Point",
      fields = List("x", "y"),
      methods = List(),
      shape = Some(Type.Shape[Clean](
        List(
          FieldType("x", Type.Number(DummyRange), DummyRange),
          FieldType("y", Type.Number(DummyRange), DummyRange),
        ),
        List(),
        DummyRange
      )),
      range = DummyRange
    )
    val decls = List(
      Decl[Clean]("px", Expr.Num(3.0, DummyRange), DummyRange),
      Decl[Clean]("py", Expr.Num(4.0, DummyRange), DummyRange),
      Decl[Clean]("p", Expr.NewInstance("Point", List("px", "py"), DummyRange), DummyRange)
    )
    val stmts = List(
      Stmt.Assign[Clean]("px", Expr.Num(300.0, DummyRange), DummyRange)
    )
    val expr = Expr.GetField[Clean]("p", "z", DummyRange)
    val prog = fullProgram(
        List(pointClass), decls, stmts, expr
      )
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, RuntimeError.FieldNotFound)
  }

  test("Object creation with wrong number of Fields") {
    val pointClass = Class[Clean](
      cname = "Point",
      fields = List("x", "y"),
      methods = List(),
      shape = None,
      range = DummyRange
    )
    val decls = List(
      Decl[Clean]("px", Expr.Num(3.0, DummyRange), DummyRange),
      Decl[Clean]("py", Expr.Num(4.0, DummyRange), DummyRange),
      Decl[Clean]("p", Expr.NewInstance("Point", List("px"), DummyRange), DummyRange)
    )
    val expr = Expr.GetField[Clean]("p", "x", DummyRange)
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
      shape = None,
      range = DummyRange
    )
    val decls = List(
      Decl[Clean]("px", Expr.Num(3.0, DummyRange), DummyRange),
      Decl[Clean]("py", Expr.Num(4.0, DummyRange), DummyRange),
      Decl[Clean]("p", Expr.NewInstance("Point", List("px", "py"), DummyRange), DummyRange)
    )
    val expr = Expr.GetField[Clean]("p", "somefield", DummyRange)
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
          Decl[Clean]("tmpBase", Expr.GetField[Clean]("this", "x", DummyRange), DummyRange)
        ),
        stmts = List(),
        expr = Expr.BinOpExpr("tmpBase", BinOp.Add, "val", DummyRange),
        range = DummyRange
      ),
      range = DummyRange
    )
    val numberClass = Class[Clean](
      cname = "Number",
      fields = List("x"),
      methods = List(addMethod),
      shape = None,
      DummyRange
    )
    val decls = List(
      Decl[Clean]("base", Expr.Num(10.0, DummyRange), DummyRange),
      Decl[Clean]("num", Expr.NewInstance("Number", List("base"), DummyRange), DummyRange),
      Decl[Clean]("increment", Expr.Num(5.0, DummyRange), DummyRange)
    )
    val expr = Expr.CallMethod[Clean]("num", "add", List("increment"), DummyRange)
    val prog = programWithClasses(List(numberClass), decls, expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 15.0)
  }

  test("Object method call for instance of Typed Class") {
    val addMethod = Method[Clean](
      mname = "add",
      params = List("val"),
      progb = ProgBlock(
        decls = List(
          Decl[Clean]("tmpBase", Expr.GetField[Clean]("this", "x", DummyRange), DummyRange)
        ),
        stmts = List(),
        expr = Expr.BinOpExpr("tmpBase", BinOp.Add, "val", DummyRange),
        DummyRange
      ),
      DummyRange
    )
    val numberClass = Class[Clean](
      cname = "Number",
      fields = List("x"),
      methods = List(addMethod),
      shape = Some(
        Type.Shape[Clean](
          List(FieldType("x", Type.Number[Clean](DummyRange), DummyRange)),
          List(MethodType("add", List(Type.Number[Clean](DummyRange)), Type.Number(DummyRange), DummyRange)),
          DummyRange
        )
      ),
      DummyRange
    )
    val decls = List(
      Decl[Clean]("base", Expr.Num(10.0, DummyRange), DummyRange),
      Decl[Clean]("num", Expr.NewInstance("Number", List("base"), DummyRange), DummyRange),
      Decl[Clean]("increment", Expr.Num(5.0, DummyRange), DummyRange)
    )
    val expr = Expr.CallMethod[Clean]("num", "add", List("increment"), DummyRange)
    val prog = programWithClasses(List(numberClass), decls, expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 15.0)
  }

  test("Object method call for instance of Typed Class Violation") {
    val addMethod = Method[Clean](
      mname = "add",
      params = List("val"),
      progb = ProgBlock(
        decls = List(
          Decl[Clean]("tmpBase", Expr.GetField[Clean]("this", "x", DummyRange), DummyRange)
        ),
        stmts = List(),
        expr = Expr.BinOpExpr("tmpBase", BinOp.Add, "val", DummyRange),
        range = DummyRange
      ),
      range = DummyRange
    )
    val numberClass = Class[Clean](
      cname = "Number",
      fields = List("x"),
      methods = List(addMethod),
      shape = Some(
        Type.Shape(
          List(FieldType("x", Type.Number(DummyRange), DummyRange)),
          List(MethodType("add", List(Type.Number(DummyRange)), Type.Number(DummyRange), DummyRange)),
          DummyRange
        ),
      ),
      DummyRange
    )
    val decls = List(
      Decl[Clean]("base", Expr.Num(10.0, DummyRange), DummyRange),
      Decl[Clean]("num", Expr.NewInstance("Number", List("base"), DummyRange), DummyRange),
      Decl[Clean]("increment", Expr.Num(5.0, DummyRange), DummyRange)
    )
    val expr = Expr.CallMethod[Clean]("num", "add", List("num"), DummyRange)
    val prog = programWithClasses(List(numberClass), decls, expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, RuntimeError.MethodCallDoesntMatchProxyMethodType)
  }

  test("Object method call for instance of Typed Class Violation With Wrong Num Params") {
    val addMethod = Method[Clean](
      mname = "add",
      params = List("val"),
      progb = ProgBlock(
        decls = List(
          Decl[Clean]("tmpBase", Expr.GetField[Clean]("this", "x", DummyRange), DummyRange)
        ),
        stmts = List(),
        expr = Expr.BinOpExpr("tmpBase", BinOp.Add, "val", DummyRange),
        range = DummyRange
      ),
      range = DummyRange
    )
    val numberClass = Class[Clean](
      cname = "Number",
      fields = List("x"),
      methods = List(addMethod),
      shape = Some(
        Type.Shape(
          List(FieldType("x", Type.Number(DummyRange), DummyRange)),
          List(MethodType("add", List(Type.Number(DummyRange)), Type.Number(DummyRange), DummyRange)),
          DummyRange
        ),
      ),
      DummyRange
    )
    val decls = List(
      Decl[Clean]("base", Expr.Num(10.0, DummyRange), DummyRange),
      Decl[Clean]("num", Expr.NewInstance("Number", List("base"), DummyRange), DummyRange),
      Decl[Clean]("increment", Expr.Num(5.0, DummyRange), DummyRange)
    )
    val expr = Expr.CallMethod[Clean]("num", "add", List("increment", "base"), DummyRange)
    val prog = programWithClasses(List(numberClass), decls, expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, RuntimeError.MethodCallDoesntMatchProxyMethodType)
  }


  test("Object method call with wrong number of parameters") {
    val addMethod = Method[Clean](
      mname = "add",
      params = List("val"),
      progb = ProgBlock(
        decls = List(
          Decl[Clean]("tmpBase", Expr.GetField[Clean]("this", "x", DummyRange), DummyRange)
        ),
        stmts = List(),
        expr = Expr.BinOpExpr("tmpBase", BinOp.Add, "val", DummyRange),
        range = DummyRange
      ),
      range = DummyRange
    )
    val numberClass = Class[Clean](
      cname = "Number",
      fields = List("x"),
      methods = List(addMethod),
      shape = None,
      range = DummyRange
    )
    val decls = List(
      Decl[Clean]("base", Expr.Num(10.0, DummyRange), DummyRange),
      Decl[Clean]("num", Expr.NewInstance("Number", List("base"), DummyRange), DummyRange),
      Decl[Clean]("increment", Expr.Num(5.0, DummyRange), DummyRange)
    )
    val expr = Expr.CallMethod[Clean]("num", "add", List("increment", "increment"), DummyRange)
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
      shape = None,
      range = DummyRange
    )
    val decls = List(
      Decl[Clean]("val", Expr.Num(1.0, DummyRange), DummyRange),
      Decl[Clean]("p", Expr.NewInstance("Point", List("val"), DummyRange), DummyRange)
    )
    val expr = Expr.IsInstanceOf[Clean]("p", "Point", DummyRange)
    val prog = programWithClasses(List(pointClass), decls, expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 0.0)
  }

  // test with Typed Class
  test("IsInstanceOf returns True for Typed Class") {
    val pointClass = Class[Clean](
      cname = "Point",
      fields = List("x"),
      methods = List(),
      shape = Some(Type.Shape(
        List(
          FieldType("x", Type.Number(DummyRange), DummyRange)
        ),
        List(),
        DummyRange
      )),
      range = DummyRange
    )
    val decls = List(
      Decl[Clean]("val", Expr.Num(1.0, DummyRange), DummyRange),
      Decl[Clean]("p", Expr.NewInstance("Point", List("val"), DummyRange), DummyRange)
    )
    val expr = Expr.IsInstanceOf[Clean]("p", "Point", DummyRange)
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
      shape = None,
      range = DummyRange
    )
    val decls = List(
      Decl[Clean]("val", Expr.Num(1.0, DummyRange), DummyRange),
      Decl[Clean]("p", Expr.NewInstance("Point", List("val"), DummyRange), DummyRange),
      Decl[Clean]("lhs", Expr.IsInstanceOf[Clean]("p", "Circle", DummyRange), DummyRange),
      Decl[Clean]("rhs", Expr.IsInstanceOf[Clean]("val", "Circle", DummyRange), DummyRange),
    )
    val expr = Expr.BinOpExpr[Clean]("lhs", BinOp.Add, "rhs", DummyRange)
    val prog = programWithClasses(List(pointClass), decls, expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 2.0)
  }

  test("IsInstanceOf returns false for incorrect type") {
    val pointClass = Class[Clean](
      cname = "Point",
      fields = List("x"),
      methods = List(),
      shape = Some(Type.Shape(
        List(
          FieldType("x", Type.Number(DummyRange), DummyRange)
        ),
        List(),
        DummyRange
      )),
      range = DummyRange
    )
    val decls = List(
      Decl[Clean]("val", Expr.Num(1.0, DummyRange), DummyRange),
      Decl[Clean]("p", Expr.NewInstance("Point", List("val"), DummyRange), DummyRange),
      Decl[Clean]("lhs", Expr.IsInstanceOf[Clean]("p", "Circle", DummyRange), DummyRange),
      Decl[Clean]("rhs", Expr.IsInstanceOf[Clean]("val", "Circle", DummyRange), DummyRange),
    )
    val expr = Expr.BinOpExpr[Clean]("lhs", BinOp.Add, "rhs", DummyRange)
    val prog = programWithClasses(List(pointClass), decls, expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 2.0)
  }


  // Test Case 12: Assignment statement
  test("Variable assignment updates value") {
    val decls = List(Decl[Clean]("x", Expr.Num(5.0, DummyRange), DummyRange))
    val stmts = List(Stmt.Assign[Clean]("x", Expr.Num(10.0, DummyRange), DummyRange))
    val expr = Expr.Var[Clean]("x", DummyRange)
    val prog = Program[Clean](
      clss = List(),
      progb = ProgBlock(decls = decls, stmts = stmts, expr = expr, range = DummyRange),
      range = DummyRange
    )
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
      shape = None,
      range = DummyRange
    )
    val decls = List(
      Decl[Clean]("px", Expr.Num(1.0, DummyRange), DummyRange),
      Decl[Clean]("py", Expr.Num(2.0, DummyRange), DummyRange),
      Decl[Clean]("p", Expr.NewInstance("Point", List("px", "py"), DummyRange), DummyRange)
    )
    val stmts = List(Stmt.FieldAssign[Clean]("p", "x", Expr.Num(99.0, DummyRange), DummyRange))
    val expr = Expr.GetField[Clean]("p", "x", DummyRange)
    val prog = Program[Clean](
      clss = List(pointClass),
      progb = ProgBlock(decls = decls, stmts = stmts, expr = expr, range = DummyRange),
      range = DummyRange
    )
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 99.0)
  }

  test("Field assignment updates object field Typed Class") {
    val pointClass = Class[Clean](
      cname = "Point",
      fields = List("x", "y"),
      methods = List(),
      shape = Some(Type.Shape(
        List(
          FieldType("x", Type.Number(DummyRange), DummyRange),
          FieldType("y", Type.Number(DummyRange), DummyRange)
        ),
        List(),
        DummyRange
      )),
      range = DummyRange
    )
    val decls = List(
      Decl[Clean]("px", Expr.Num(1.0, DummyRange), DummyRange),
      Decl[Clean]("py", Expr.Num(2.0, DummyRange), DummyRange),
      Decl[Clean]("p", Expr.NewInstance("Point", List("px", "py"), DummyRange), DummyRange)
    )
    val stmts = List(Stmt.FieldAssign[Clean]("p", "x", Expr.Num(99.0, DummyRange), DummyRange))
    val expr = Expr.GetField[Clean]("p", "x", DummyRange)
    val prog = Program[Clean](
      clss = List(pointClass),
      progb = ProgBlock(decls = decls, stmts = stmts, expr = expr, range = DummyRange),
      range = DummyRange
    )
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 99.0)
  }
  
  test("Field assignment updates object field Typed Class Violation") {
    val pointClass = Class[Clean](
      cname = "Point",
      fields = List("x", "y"),
      methods = List(),
      shape = Some(Type.Shape(
        List(
          FieldType("x", Type.Number(DummyRange), DummyRange),
          FieldType("y", Type.Number(DummyRange), DummyRange)
        ),
        List(),
        DummyRange
      )),
      range = DummyRange
    )
    val decls = List(
      Decl[Clean]("px", Expr.Num(1.0, DummyRange), DummyRange),
      Decl[Clean]("py", Expr.Num(2.0, DummyRange), DummyRange),
      Decl[Clean]("p", Expr.NewInstance("Point", List("px", "py"), DummyRange), DummyRange)
    )
    val stmts = List(
      Stmt.FieldAssign[Clean](
        "p",
        "x",
        Expr.NewInstance("Point", List("px", "py"), DummyRange),
        DummyRange
      )
    )
    val expr = Expr.GetField[Clean]("p", "x", DummyRange)
    val prog = Program[Clean](
      clss = List(pointClass),
      progb = ProgBlock(decls = decls, stmts = stmts, expr = expr, range = DummyRange),
      range = DummyRange
    )
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, RuntimeError.ValDoesntConformToExpType)
  }
    
  test("Field assignment tries updates object field that does not exist") {
    val pointClass = Class[Clean](
      cname = "Point",
      fields = List("x", "y"),
      methods = List(),
      shape = None,
      range = DummyRange
    )
    val decls = List(
      Decl[Clean]("px", Expr.Num(1.0, DummyRange), DummyRange),
      Decl[Clean]("py", Expr.Num(2.0, DummyRange), DummyRange),
      Decl[Clean]("p", Expr.NewInstance("Point", List("px", "py"), DummyRange), DummyRange)
    )
    val stmts = List(Stmt.FieldAssign[Clean]("p", "somefield", Expr.Num(99.0, DummyRange), DummyRange))
    val expr = Expr.GetField[Clean]("p", "x", DummyRange)
    val prog = Program[Clean](
      clss = List(pointClass),
      progb = ProgBlock(decls = decls, stmts = stmts, expr = expr, range = DummyRange),
      range = DummyRange
    )
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, RuntimeError.FieldNotFound)
  }

  test("Objects fail to conform to Proxy types") {

    def maketClassWithType(name : String, uclassType : CleanShapeType) = 
      Class[Clean](
        cname = "Maker" ++ name ++ ".into.Body",
        fields = List(),
        methods = List(
          Method[Clean](
            mname = "run",
            params = List("val"),
            progb = ProgBlock(
              decls = List(),
              stmts = List(),
              expr = Expr.NewInstance("Untyped", List("val", "val"), DummyRange),
              range = DummyRange
            ),
            range = DummyRange
          )
        ),
        shape = Some(Type.Shape(
          List(),
          List(
            MethodType("run", List(Type.Number(DummyRange)), uclassType, DummyRange)
          ),
          DummyRange
        )),
        range = DummyRange
      )

    val uClass = Class[Clean](
      cname = "Untyped",
      fields = List("x", "y"),
      methods = List(
        Method[Clean](
          mname = "catch",
          params = List("val"),
          progb = ProgBlock(
            decls = List(),
            stmts = List(),
            expr = Expr.Var("val", DummyRange),
            range = DummyRange
          ),
          range = DummyRange
        )
      ),
      shape = None,
      range = DummyRange
    )

    val uClassBadType1 : CleanShapeType= 
      Type.Shape[Clean](
        List(
          FieldType("x", Type.Number(DummyRange), DummyRange),
          FieldType("y", Type.Number(DummyRange), DummyRange),
          FieldType("z", Type.Number(DummyRange), DummyRange)
        ),
        List(MethodType("catch", List(Type.Number(DummyRange)), Type.Number(DummyRange), DummyRange)),
        DummyRange
      )

    val uClassBadType2 : CleanShapeType= 
      Type.Shape[Clean](
        List(
          FieldType("x", Type.Number(DummyRange), DummyRange),
          FieldType("y", Type.Shape[Clean](Nil, Nil, DummyRange), DummyRange)
        ),
        List(MethodType("catch", List(Type.Number(DummyRange)), Type.Number(DummyRange), DummyRange)),
        DummyRange
      )

    val uClassBadType3 : CleanShapeType= 
      Type.Shape[Clean](
        List(
          FieldType("x", Type.Number(DummyRange), DummyRange),
          FieldType("y", Type.Number(DummyRange), DummyRange)
        ),
        List(MethodType("not a catch", List(Type.Number(DummyRange)), Type.Number(DummyRange), DummyRange)),
        DummyRange
      )

    val uClassBadType4 : CleanShapeType= 
      Type.Shape[Clean](
        List(
          FieldType("x", Type.Number(DummyRange), DummyRange),
          FieldType("y", Type.Number(DummyRange), DummyRange),
        ),
        List(MethodType("catch", List(), Type.Number(DummyRange), DummyRange)),
        DummyRange
      )

    val clss = List(
      uClass, 
      maketClassWithType("Bad1", uClassBadType1), 
      maketClassWithType("Bad2", uClassBadType2), 
      maketClassWithType("Bad3", uClassBadType3), 
      maketClassWithType("Bad4", uClassBadType4), 
    )
   
    val decls = List(
      Decl[Clean]("px", Expr.Num(1.0, DummyRange), DummyRange),
      Decl[Clean]("pBad1", Expr.NewInstance("MakerBad1.into.Body", List(), DummyRange), DummyRange),
      Decl[Clean]("pBad2", Expr.NewInstance("MakerBad2.into.Body", List(), DummyRange), DummyRange),
      Decl[Clean]("pBad3", Expr.NewInstance("MakerBad3.into.Body", List(), DummyRange), DummyRange),
      Decl[Clean]("pBad4", Expr.NewInstance("MakerBad4.into.Body", List(), DummyRange), DummyRange)
    )

    def runAndAssertConformError(objRef : String, expectedErr : RuntimeError) : Unit =
      val prog = programWithClasses(
        clss,
        decls,
        Expr.CallMethod[Clean](objRef, "run", List("px"), DummyRange)
      )
      val machine = CESKMachine(prog)
      val result = machine.run
      assertEquals(result, expectedErr)

    runAndAssertConformError("pBad1", RuntimeError.FieldNamesDontConformToProxyShape)
    runAndAssertConformError("pBad2", RuntimeError.FieldValsDontConformToProxyShape)
    runAndAssertConformError("pBad3", RuntimeError.MethodNamesDontConformToProxyShape)
    runAndAssertConformError("pBad4", RuntimeError.MethodParamsDontConformToProxyShape)
  }

  test("Proxy to conforms an outer to Proxy type") {

    val catchClassNumType : CleanShapeType=
      Type.Shape[Clean](
        List(),
        List(MethodType("catch", List(Type.Number(DummyRange)), Type.Number(DummyRange), DummyRange)),
        DummyRange
      )
    
    val catchClassDummyType : CleanShapeType=
      Type.Shape[Clean](
        List(),
        List(MethodType("catch", List(Type.Shape[Clean](Nil, Nil, DummyRange)), Type.Shape[Clean](Nil, Nil, DummyRange), DummyRange)),
        DummyRange
      )

    val outerClass =
      Class[Clean](
        cname = "Outer",
        fields = List(),
        methods = List(
          Method[Clean](
            mname = "run",
            params = List("val"),
            progb = ProgBlock(
              decls = List(),
              stmts = List(),
              expr = Expr.Var("val", DummyRange),
              range = DummyRange
            ),
            range = DummyRange
          )
        ),
        shape = Some(Type.Shape(
          List(),
          List(
            MethodType("run", List(catchClassNumType), catchClassNumType, DummyRange)
          ),
          DummyRange
        )),
        range = DummyRange
      )

    def maketClassWithType(name : String, catchClassType : CleanShapeType) = 
      Class[Clean](
        cname = "Catch" ++ name,
        fields = List(),
        methods = List(
          Method[Clean](
            mname = "catch",
            params = List("val"),
            progb = ProgBlock(
              decls = List(),
              stmts = List(),
              expr = Expr.Var("val", DummyRange),
              range = DummyRange
            ),
            range = DummyRange
          )
        ),
        shape = Some(catchClassType),
        range = DummyRange
      )

    val clss = List(
      outerClass,
      maketClassWithType("Num", catchClassNumType), 
      maketClassWithType("Obj", catchClassDummyType)
    )
   
    val decls = List(
      Decl[Clean]("pOuter", Expr.NewInstance("Outer", List(), DummyRange), DummyRange),
      Decl[Clean]("pCatchNum", Expr.NewInstance("CatchNum", List(), DummyRange), DummyRange),
      Decl[Clean]("pCatchObj", Expr.NewInstance("CatchObj", List(), DummyRange), DummyRange)
    )

    def runAndAssertConform(argVRef : String, expected : CESKValue | RuntimeError) : Unit =
      val prog = programWithClasses(
        clss,
        decls,
        Expr.CallMethod[Clean]("pOuter", "run", List(argVRef), DummyRange)
      )
      val machine = CESKMachine(prog)
      val result = machine.run
      assertEquals(result, expected)

    runAndAssertConform("pCatchNum", ProxyVal(ObjectVal("CatchNum", MutableMap()), catchClassNumType))
    runAndAssertConform("pCatchObj", RuntimeError.MethodCallDoesntMatchProxyMethodType)
    // runAndAssertConform("pCatchObj", RuntimeError.ProxyValDoesntConformToProxyShape)
  }

  // Test Case 14: If-else conditional - true branch
  test("If-else takes true branch when condition is truthy") {
    val decls = List(
      Decl[Clean]("condition", Expr.Num(1.0, DummyRange), DummyRange),
      Decl[Clean]("result", Expr.Num(0.0, DummyRange), DummyRange)
    )
    val stmts = List(
      Stmt.Ifelse[Clean](
        guard = Expr.Var("condition", DummyRange),
        tbranch = StmtBlock.One(Stmt.Assign("result", Expr.Num(100.0, DummyRange), DummyRange), DummyRange),
        ebranch = StmtBlock.One(Stmt.Assign("result", Expr.Num(200.0, DummyRange), DummyRange), DummyRange),
        DummyRange
      )
    )
    val expr = Expr.Var[Clean]("result", DummyRange)
    val prog = Program[Clean](
      clss = List(),
      progb = ProgBlock(decls = decls, stmts = stmts, expr = expr, range = DummyRange),
      range = DummyRange
    )
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 200.0)
  }

  // Test Case 15: If-else conditional - false branch
  test("If-else takes false branch when condition is falsy") {
    val decls = List(
      Decl[Clean]("condition", Expr.Num(0.0, DummyRange), DummyRange),
      Decl[Clean]("result", Expr.Num(0.0, DummyRange), DummyRange)
    )
    val stmts = List(
      Stmt.Ifelse[Clean](
        guard = Expr.Var("condition", DummyRange),
        tbranch = StmtBlock.One(Stmt.Assign("result", Expr.Num(100.0, DummyRange), DummyRange), DummyRange),
        ebranch = StmtBlock.One(Stmt.Assign("result", Expr.Num(200.0, DummyRange), DummyRange), DummyRange),
        DummyRange
      )
    )
    val expr = Expr.Var[Clean]("result", DummyRange)
    val prog = Program[Clean](
      clss = List(),
      progb = ProgBlock(decls = decls, stmts = stmts, expr = expr, range = DummyRange),
      range = DummyRange
    )
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 100.0)
  }

  test("If else has object at guard position") {
    val pointClass = Class[Clean](
      cname = "Point",
      fields = List("x", "y"),
      methods = List(),
      shape = None,
      range = DummyRange
    )
    val decls = List(
      Decl[Clean]("px", Expr.Num(1.0, DummyRange), DummyRange),
      Decl[Clean]("py", Expr.Num(2.0, DummyRange), DummyRange),
      Decl[Clean]("p", Expr.NewInstance("Point", List("px", "py"), DummyRange), DummyRange)
    )
    val stmts = List(
      Stmt.Ifelse[Clean](
        guard = Expr.Var("p", DummyRange),
        tbranch = StmtBlock.One(Stmt.Assign("px", Expr.Num(100.0, DummyRange), DummyRange), DummyRange),
        ebranch = StmtBlock.One(Stmt.Assign("px", Expr.Num(200.0, DummyRange), DummyRange), DummyRange),
        DummyRange
      )
    )
    val expr = Expr.Var[Clean]("px", DummyRange)
    val prog = Program[Clean](
      clss = List(pointClass),
      progb = ProgBlock(decls = decls, stmts = stmts, expr = expr, range = DummyRange),
      range = DummyRange
    )
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 200.0)
  }


  // Test Case 16: While loop
  test("While loop executes correctly") {
    val decls = List(
      Decl[Clean]("counter", Expr.Num(0.0, DummyRange), DummyRange),
      Decl[Clean]("one", Expr.Num(1.0, DummyRange), DummyRange),
      Decl[Clean]("limit", Expr.Num(3.0, DummyRange), DummyRange)
    )
    val stmts = List(
      Stmt.While[Clean](
        guard = Expr.Var("counter", DummyRange),
        body = StmtBlock.One[Clean](
          Stmt.Assign(
            "counter",
            Expr.BinOpExpr[Clean]("one", BinOp.Add, "counter", DummyRange),
            DummyRange
          ),
          DummyRange
        ),
        DummyRange
      )
    )
    val expr = Expr.Var[Clean]("limit", DummyRange)
    val prog = Program[Clean](
      clss = List(),
      progb = ProgBlock(decls = decls, stmts = stmts, expr = expr, range = DummyRange),
      range = DummyRange
    )
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 3.0)
  }

  test("While has object at guard position") {
    val pointClass = Class[Clean](
      cname = "Point",
      fields = List("x", "y"),
      methods = List(),
      shape = None,
      range = DummyRange
    )
    val decls = List(
      Decl[Clean]("px", Expr.Num(1.0, DummyRange), DummyRange),
      Decl[Clean]("py", Expr.Num(2.0, DummyRange), DummyRange),
      Decl[Clean]("p", Expr.NewInstance("Point", List("px", "py"), DummyRange), DummyRange)
    )
    val stmts = List(
      Stmt.While[Clean](
        guard = Expr.Var("p", DummyRange),
        body = StmtBlock.One[Clean](
          Stmt.Assign(
            "px",
            Expr.BinOpExpr[Clean]("py", BinOp.Add, "py", DummyRange),
            DummyRange
          ),
          DummyRange
        ),
        DummyRange
      )
    )
    val expr = Expr.Var[Clean]("px", DummyRange)
    val prog = Program[Clean](
      clss = List(pointClass),
      progb = ProgBlock(decls = decls, stmts = stmts, expr = expr, range = DummyRange),
      range = DummyRange
    )
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 1.0)
  }

  // Test Case 17: Error - accessing field on non-object
  test("Field access on numeric value produces error") {
    val decls = List(Decl[Clean]("x", Expr.Num(42.0, DummyRange), DummyRange))
    val expr = Expr.GetField[Clean]("x", "someField", DummyRange)
    val prog = programWithDecls(decls, expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assert(result.isInstanceOf[RuntimeError.ValNotAnObject.type])
  }

  // Test Case 18: Error - method call on non-object
  test("Method call on numeric value produces error") {
    val decls = List(Decl[Clean]("x", Expr.Num(42.0, DummyRange), DummyRange))
    val expr = Expr.CallMethod[Clean]("x", "someMethod", List(), DummyRange)
    val prog = programWithDecls(decls, expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assert(result.isInstanceOf[RuntimeError.ValNotAnObject.type])
  }

  // Test Case 19: Complex program with multiple declarations and operations
  test("Complex program with multiple operations") {
    val decls = List(
      Decl[Clean]("a", Expr.Num(10.0, DummyRange), DummyRange),
      Decl[Clean]("b", Expr.Num(5.0, DummyRange), DummyRange),
      Decl[Clean]("sum", Expr.BinOpExpr("a", BinOp.Add, "b", DummyRange), DummyRange),
      Decl[Clean]("product", Expr.BinOpExpr("sum", BinOp.Add, "a", DummyRange), DummyRange)
    )
    val expr = Expr.Var[Clean]("product", DummyRange)
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
        expr = Expr.GetField("this", "x", DummyRange),
        range = DummyRange
      ),
      range = DummyRange
    )
    val pointClass = Class[Clean](
      cname = "Point",
      fields = List("x", "y"),
      methods = List(getXMethod),
      shape = None,
      range = DummyRange
    )
    val decls = List(
      Decl[Clean]("px", Expr.Num(42.0, DummyRange), DummyRange),
      Decl[Clean]("py", Expr.Num(24.0, DummyRange), DummyRange),
      Decl[Clean]("p", Expr.NewInstance("Point", List("px", "py"), DummyRange), DummyRange)
    )
    val expr = Expr.CallMethod[Clean]("p", "getX", List(), DummyRange)
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
