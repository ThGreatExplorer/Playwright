package test

import munit.FunSuite
import ast._
import util.{UnreachableStateException, UnreachablePatternMatch}
import cesk.{CESKMachine, Store, Env, KontStack, ProgFrame, RuntimeError}
import cesk.CESKValue

class CESKTests extends FunSuite {

  // Helper method to create a simple program with just an expression
  def simpleProgram(expr: CleanExpr): CleanProgram = 
    Program[Clean](clss = List(), decls = List(), stmts = List(), expr = expr)

  // Helper method to create a program with declarations
  def programWithDecls(decls: List[CleanDecl], expr: CleanExpr): CleanProgram =
    Program[Clean](clss = List(), decls = decls, stmts = List(), expr = expr)

  // Helper method to create a program with classes
  def programWithClasses(classes: List[CleanClass], decls: List[CleanDecl], expr: CleanExpr): CleanProgram =
    Program[Clean](clss = classes, decls = decls, stmts = List(), expr = expr)

  test("Test malformed input triggers exception") {
    val cleanProgram = Program[Clean](
      clss = List(),
      decls = List(),
      stmts = List(),
      expr = Expr.BinOpExpr(VarRef("foo"), BinOp.Div, VarRef("foo"))
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
    assertEquals(kont2.top.toString(), "(ProgFrame(List(),List(),()),Map())")
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
    val decl = Decl[Clean](Name("x"), Expr.Num(10.0))
    val expr = Expr.Var[Clean](VarRef("x"))
    val prog = programWithDecls(List(decl), expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 10.0)
  }

  // Test Case 3: Binary operations - Addition
  test("Binary addition of two variables") {
    val decls = List(
      Decl[Clean](Name("a"), Expr.Num(5.0)),
      Decl[Clean](Name("b"), Expr.Num(3.0))
    )
    val expr = Expr.BinOpExpr[Clean](VarRef("a"), BinOp.Add, VarRef("b"))
    val prog = programWithDecls(decls, expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 8.0)
  }

  // Test Case 4: Binary operations - Division
  test("Binary division of two variables") {
    val decls = List(
      Decl[Clean](Name("x"), Expr.Num(15.0)),
      Decl[Clean](Name("y"), Expr.Num(3.0))
    )
    val expr = Expr.BinOpExpr[Clean](VarRef("x"), BinOp.Div, VarRef("y"))
    val prog = programWithDecls(decls, expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 5.0)
  }

  // Test Case 5: Division by zero error
  test("Division by zero produces runtime error") {
    val decls = List(
      Decl[Clean](Name("x"), Expr.Num(10.0)),
      Decl[Clean](Name("y"), Expr.Num(0.0))
    )
    val expr = Expr.BinOpExpr[Clean](VarRef("x"), BinOp.Div, VarRef("y"))
    val prog = programWithDecls(decls, expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assert(result.isInstanceOf[RuntimeError.DivisionByZero])
  }

  // Test Case 6: Equality comparison - true case
  test("Equality comparison returns truthy for equal values") {
    val decls = List(
      Decl[Clean](Name("a"), Expr.Num(7.0)),
      Decl[Clean](Name("b"), Expr.Num(7.0))
    )
    val expr = Expr.BinOpExpr[Clean](VarRef("a"), BinOp.Equals, VarRef("b"))
    val prog = programWithDecls(decls, expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 0.0)
  }

  // Test Case 7: Equality comparison - false case
  test("Equality comparison returns falsy for unequal values") {
    val decls = List(
      Decl[Clean](Name("a"), Expr.Num(5.0)),
      Decl[Clean](Name("b"), Expr.Num(3.0))
    )
    val expr = Expr.BinOpExpr[Clean](VarRef("a"), BinOp.Equals, VarRef("b"))
    val prog = programWithDecls(decls, expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 1.0)
  }

  // Test Case 8: Equality Comparison among objects and objects vs Nums
  test("Equality Comparison among objects and objects vs Nums") {
    val pointClass = Class[Clean](
      cname = Name("Point"),
      fields = List(Name("x"), Name("y")),
      methods = List()
    )
    val decls = List(
      Decl[Clean](Name("px"), Expr.Num(3.0)),
      Decl[Clean](Name("py"), Expr.Num(4.0)),
      Decl[Clean](Name("pone"), Expr.NewInstance(Name("Point"), List(VarRef("px"), VarRef("py")))),
      Decl[Clean](Name("ptwo"), Expr.NewInstance(Name("Point"), List(VarRef("px"), VarRef("py"))))
    )

    def runAndAssertProg(expr : Expr[Clean], expectedResult : CESKValue | RuntimeError): Unit =
      val prog = programWithClasses(List(pointClass), decls, expr)
      val machine = CESKMachine(prog)
      val result = machine.run
      assertEquals(result, expectedResult)

    // check referential equality
    val expr = Expr.BinOpExpr[Clean](VarRef("pone"), BinOp.Equals, VarRef("pone"))
    runAndAssertProg(expr, 0.0)
    // check structural equality
    val expr2 = Expr.BinOpExpr[Clean](VarRef("pone"), BinOp.Equals, VarRef("ptwo"))
    runAndAssertProg(expr2, 0.0)
    // check object, num comparison returns false
    val expr3 = Expr.BinOpExpr[Clean](VarRef("pone"), BinOp.Equals, VarRef("px"))
    runAndAssertProg(expr3, 1.0)
    val expr4 = Expr.BinOpExpr[Clean](VarRef("px"), BinOp.Equals, VarRef("pone"))
    runAndAssertProg(expr4, 1.0)
  }

  // Test Case 8: BinOps Invalid Comparisons between Num and Obj
  test("BinOps Invalid Comparisons between Num and Obj") {
    val pointClass = Class[Clean](
      cname = Name("Point"),
      fields = List(Name("x"), Name("y")),
      methods = List()
    )
    val decls = List(
      Decl[Clean](Name("px"), Expr.Num(3.0)),
      Decl[Clean](Name("py"), Expr.Num(4.0)),
      Decl[Clean](Name("pone"), Expr.NewInstance(Name("Point"), List(VarRef("px"), VarRef("py")))),
      Decl[Clean](Name("ptwo"), Expr.NewInstance(Name("Point"), List(VarRef("px"), VarRef("py"))))
    )

    val expr = Expr.BinOpExpr[Clean](VarRef("pone"), BinOp.Add, VarRef("pone"))

    def runAndAssertProg(expr : Expr[Clean], expectedResult : CESKValue | RuntimeError): Unit =
      val prog = programWithClasses(List(pointClass), decls, expr)
      val machine = CESKMachine(prog)
      val result = machine.run
      assertEquals(result, expectedResult)

    runAndAssertProg(expr, RuntimeError.InvalidVarType("Binop attempted on a non-numeric value."))
    val expr2 = Expr.BinOpExpr[Clean](VarRef("pone"), BinOp.Add, VarRef("px"))
    runAndAssertProg(expr2, RuntimeError.InvalidVarType("Binop attempted on a non-numeric value."))
    val expr3 = Expr.BinOpExpr[Clean](VarRef("px"), BinOp.Add, VarRef("pone"))
    runAndAssertProg(expr3, RuntimeError.InvalidVarType("Binop attempted on a non-numeric value."))
  }

  // Test Case 8: Object creation and field access
  test("Object creation and field access") {
    val pointClass = Class[Clean](
      cname = Name("Point"),
      fields = List(Name("x"), Name("y")),
      methods = List()
    )
    val decls = List(
      Decl[Clean](Name("px"), Expr.Num(3.0)),
      Decl[Clean](Name("py"), Expr.Num(4.0)),
      Decl[Clean](Name("p"), Expr.NewInstance(Name("Point"), List(VarRef("px"), VarRef("py"))))
    )
    val expr = Expr.GetField[Clean](VarRef("p"), Name("x"))
    val prog = programWithClasses(List(pointClass), decls, expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 3.0)
  }

  test("Object creation with wrong number of Fields") {
    val pointClass = Class[Clean](
      cname = Name("Point"),
      fields = List(Name("x"), Name("y")),
      methods = List()
    )
    val decls = List(
      Decl[Clean](Name("px"), Expr.Num(3.0)),
      Decl[Clean](Name("py"), Expr.Num(4.0)),
      Decl[Clean](Name("p"), Expr.NewInstance(Name("Point"), List(VarRef("px"))))
    )
    val expr = Expr.GetField[Clean](VarRef("p"), Name("x"))
    val prog = programWithClasses(List(pointClass), decls, expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, RuntimeError.NewInstWrongFieldCount)
  }

  // Test Case 9: Object method call
  test("Object method call with parameters") {
    val addMethod = Method[Clean](
      mname = Name("add"),
      params = List(Name("val")),
      decls = List(
        Decl[Clean](Name("tmpBase"), Expr.GetField[Clean](VarRef("this"), Name("x")))
      ),
      stmts = List(),
      expr = Expr.BinOpExpr(VarRef("tmpBase"), BinOp.Add, VarRef("val"))
    )
    val numberClass = Class[Clean](
      cname = Name("Number"),
      fields = List(Name("x")),
      methods = List(addMethod)
    )
    val decls = List(
      Decl[Clean](Name("base"), Expr.Num(10.0)),
      Decl[Clean](Name("num"), Expr.NewInstance(Name("Number"), List(VarRef("base")))),
      Decl[Clean](Name("increment"), Expr.Num(5.0))
    )
    val expr = Expr.CallMethod[Clean](VarRef("num"), Name("add"), List(VarRef("increment")))
    val prog = programWithClasses(List(numberClass), decls, expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 15.0)
  }

  test("Object method call with wrong number of parameters") {
    val addMethod = Method[Clean](
      mname = Name("add"),
      params = List(Name("val")),
      decls = List(
        Decl[Clean](Name("tmpBase"), Expr.GetField[Clean](VarRef("this"), Name("x")))
      ),
      stmts = List(),
      expr = Expr.BinOpExpr(VarRef("tmpBase"), BinOp.Add, VarRef("val"))
    )
    val numberClass = Class[Clean](
      cname = Name("Number"),
      fields = List(Name("x")),
      methods = List(addMethod)
    )
    val decls = List(
      Decl[Clean](Name("base"), Expr.Num(10.0)),
      Decl[Clean](Name("num"), Expr.NewInstance(Name("Number"), List(VarRef("base")))),
      Decl[Clean](Name("increment"), Expr.Num(5.0))
    )
    val expr = Expr.CallMethod[Clean](VarRef("num"), Name("add"), List(VarRef("increment"), VarRef("increment")))
    val prog = programWithClasses(List(numberClass), decls, expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, RuntimeError.MethodCallWrongArgCount)
  }
  

  // Test Case 10: IsInstanceOf check - positive case
  test("IsInstanceOf returns true for correct type") {
    val pointClass = Class[Clean](
      cname = Name("Point"),
      fields = List(Name("x")),
      methods = List()
    )
    val decls = List(
      Decl[Clean](Name("val"), Expr.Num(1.0)),
      Decl[Clean](Name("p"), Expr.NewInstance(Name("Point"), List(VarRef("val"))))
    )
    val expr = Expr.IsInstanceOf[Clean](VarRef("p"), Name("Point"))
    val prog = programWithClasses(List(pointClass), decls, expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 0.0)
  }

  // Test Case 11: IsInstanceOf check - negative case
  test("IsInstanceOf returns false for incorrect type") {
    val pointClass = Class[Clean](
      cname = Name("Point"),
      fields = List(Name("x")),
      methods = List()
    )
    val decls = List(
      Decl[Clean](Name("val"), Expr.Num(1.0)),
      Decl[Clean](Name("p"), Expr.NewInstance(Name("Point"), List(VarRef("val"))))
    )
    val expr = Expr.IsInstanceOf[Clean](VarRef("p"), Name("Circle"))
    val prog = programWithClasses(List(pointClass), decls, expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 1.0)
  }

  // Test Case 12: Assignment statement
  test("Variable assignment updates value") {
    val decls = List(Decl[Clean](Name("x"), Expr.Num(5.0)))
    val stmts = List(Stmt.Assign[Clean](VarRef("x"), Expr.Num(10.0)))
    val expr = Expr.Var[Clean](VarRef("x"))
    val prog = Program[Clean](clss = List(), decls = decls, stmts = stmts, expr = expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 10.0)
  }

  // Test Case 13: Field assignment
  test("Field assignment updates object field") {
    val pointClass = Class[Clean](
      cname = Name("Point"),
      fields = List(Name("x"), Name("y")),
      methods = List()
    )
    val decls = List(
      Decl[Clean](Name("px"), Expr.Num(1.0)),
      Decl[Clean](Name("py"), Expr.Num(2.0)),
      Decl[Clean](Name("p"), Expr.NewInstance(Name("Point"), List(VarRef("px"), VarRef("py"))))
    )
    val stmts = List(Stmt.FieldAssign[Clean](VarRef("p"), Name("x"), Expr.Num(99.0)))
    val expr = Expr.GetField[Clean](VarRef("p"), Name("x"))
    val prog = Program[Clean](clss = List(pointClass), decls = decls, stmts = stmts, expr = expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 99.0)
  }

  // Test Case 14: If-else conditional - true branch
  test("If-else takes true branch when condition is truthy") {
    val decls = List(
      Decl[Clean](Name("condition"), Expr.Num(1.0)),
      Decl[Clean](Name("result"), Expr.Num(0.0))
    )
    val stmts = List(
      Stmt.Ifelse[Clean](
        guard = Expr.Var(VarRef("condition")),
        tbranch = Block.One(Stmt.Assign(VarRef("result"), Expr.Num(100.0))),
        ebranch = Block.One(Stmt.Assign(VarRef("result"), Expr.Num(200.0)))
      )
    )
    val expr = Expr.Var[Clean](VarRef("result"))
    val prog = Program[Clean](clss = List(), decls = decls, stmts = stmts, expr = expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 200.0)
  }

  // Test Case 15: If-else conditional - false branch
  test("If-else takes false branch when condition is falsy") {
    val decls = List(
      Decl[Clean](Name("condition"), Expr.Num(0.0)),
      Decl[Clean](Name("result"), Expr.Num(0.0))
    )
    val stmts = List(
      Stmt.Ifelse[Clean](
        guard = Expr.Var(VarRef("condition")),
        tbranch = Block.One(Stmt.Assign(VarRef("result"), Expr.Num(100.0))),
        ebranch = Block.One(Stmt.Assign(VarRef("result"), Expr.Num(200.0)))
      )
    )
    val expr = Expr.Var[Clean](VarRef("result"))
    val prog = Program[Clean](clss = List(), decls = decls, stmts = stmts, expr = expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 100.0)
  }

  // Test Case 16: While loop
  test("While loop executes correctly") {
    val decls = List(
      Decl[Clean](Name("counter"), Expr.Num(0.0)),
      Decl[Clean](Name("one"), Expr.Num(1.0)),
      Decl[Clean](Name("limit"), Expr.Num(3.0))
    )
    val stmts = List(
      Stmt.While[Clean](
        guard = Expr.Var(VarRef("counter")),
        body = Block.One[Clean](Stmt.Assign(VarRef("counter"), 
          Expr.BinOpExpr[Clean](VarRef("one"), BinOp.Add, VarRef("counter")))),
      )
    )
    val expr = Expr.Var[Clean](VarRef("limit"))
    val prog = Program[Clean](clss = List(), decls = decls, stmts = stmts, expr = expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 3.0)
  }

  // Test Case 17: Error - accessing field on non-object
  test("Field access on numeric value produces error") {
    val decls = List(Decl[Clean](Name("x"), Expr.Num(42.0)))
    val expr = Expr.GetField[Clean](VarRef("x"), Name("someField"))
    val prog = programWithDecls(decls, expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assert(result.isInstanceOf[RuntimeError.ValNotAnObject.type])
  }

  // Test Case 18: Error - method call on non-object
  test("Method call on numeric value produces error") {
    val decls = List(Decl[Clean](Name("x"), Expr.Num(42.0)))
    val expr = Expr.CallMethod[Clean](VarRef("x"), Name("someMethod"), List())
    val prog = programWithDecls(decls, expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assert(result.isInstanceOf[RuntimeError.ValNotAnObject.type])
  }

  // Test Case 19: Complex program with multiple declarations and operations
  test("Complex program with multiple operations") {
    val decls = List(
      Decl[Clean](Name("a"), Expr.Num(10.0)),
      Decl[Clean](Name("b"), Expr.Num(5.0)),
      Decl[Clean](Name("sum"), Expr.BinOpExpr(VarRef("a"), BinOp.Add, VarRef("b"))),
      Decl[Clean](Name("product"), Expr.BinOpExpr(VarRef("sum"), BinOp.Add, VarRef("a")))
    )
    val expr = Expr.Var[Clean](VarRef("product"))
    val prog = programWithDecls(decls, expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 25.0) // (10 + 5) + 10 = 25
  }

  // Test Case 20: Method with this reference
  test("Method accessing this reference") {
    val getXMethod = Method[Clean](
      mname = Name("getX"),
      params = List(),
      decls = List(),
      stmts = List(),
      expr = Expr.GetField(VarRef("this"), Name("x"))
    )
    val pointClass = Class[Clean](
      cname = Name("Point"),
      fields = List(Name("x"), Name("y")),
      methods = List(getXMethod)
    )
    val decls = List(
      Decl[Clean](Name("px"), Expr.Num(42.0)),
      Decl[Clean](Name("py"), Expr.Num(24.0)),
      Decl[Clean](Name("p"), Expr.NewInstance(Name("Point"), List(VarRef("px"), VarRef("py"))))
    )
    val expr = Expr.CallMethod[Clean](VarRef("p"), Name("getX"), List())
    val prog = programWithClasses(List(pointClass), decls, expr)
    val machine = CESKMachine(prog)
    val result = machine.run
    assertEquals(result, 42.0)
  }

  // ----- Complex Tests which paseed validity cases -----
  val ans = List(100.0, 1.5, 10.0, 1.0, 1.0, 0.0, RuntimeError.MethodNotFound)
  ValidityTests.validCases.zip(ans).foreach(
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