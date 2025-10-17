// package test

// import munit.FunSuite
// import main.MainFuncs
// import static.Parser
// import ast._
// import util.{UnreachableStateException, UnreachablePatternMatch}
// import cesk.{CESKMachine, Store, Env, KontStack, RuntimeError}
// import cesk.ProgFrame

// class CESKTests extends FunSuite {

//   val expectedValues : List[NumVal] = List(100.0, 1.5, 10.0, 1.0, 1.0)
  
//   ValidityTests.validCases.zip(expectedValues).foreach{ case ((rawStr, cleanProgram, _), expectedVal) =>
//     test(s"Proper Validity Tests that should pass $rawStr") {
//       CESKMachine.run(cleanProgram) match
//         case n : NumVal => assertEquals(n, expectedVal)
//         case e : RuntimeError => throw new Exception("None of the above tests should fail!")
//     }
//   }

//   CESKTests.invalidCases.foreach{ case (rawStr, cleanProgram, errMsg) =>
//     test(s"Improper Tests that should fail $rawStr") {
//        CESKMachine.run(cleanProgram) match
//         case n : NumVal => throw new Exception("None of the tests should pass")
//         case e : RuntimeError => assertEquals(e, errMsg)
//     }
//   }

//   test("Test malformed input triggers exception") {
//     val cleanProgram = CleanProgram(
//         decls = List(),
//         stmts = List(),
//         expr = CleanExpr.BinOpExpr(CleanExpr.Var("foo"), BinOp.Div, CleanExpr.Var("foo"))
//     )
//     interceptMessage[UnreachablePatternMatch]("Should never happen: variable foo not found in environment") {
//       CESKMachine.run(cleanProgram)
//     }
//   }

//   test("Test Store") {
//     val store = Store()
//     assertEquals(store.toString(), "Map()")
//     interceptMessage[UnreachablePatternMatch]("Should never happen: location 1 not found in store") {
//       store.getVal(1)
//     }
//     val (store2, loc) = store.insertValAtNewLoc(2.0)
//     assertEquals(store2.toString(), f"Map($loc -> 2.0)")
//     assertEquals(store2.getVal(loc), 2.0)
//   }
  
//   test("Test Env"){
//     val env = Env()
//     interceptMessage[UnreachablePatternMatch]("Should never happen: variable x not found in environment") {
//       env.getLoc("x")
//     }
//     val env2 = env.updatedEnv("x", 1)
//     assertEquals(env2.toString(), "Map(x -> 1)")
//   }

//   test("Test KontStack"){
//     val kont = KontStack()
//     assertEquals(kont.toString(), "List()")
//     val kont2 = kont.push((ProgFrame(Nil, Nil, ()), Env()))
//     assertEquals(kont2.toString(), "List((ProgFrame(List(),List(),()),Map()))")
//     assertEquals(kont2.top.toString(), "(ProgFrame(List(),List(),()),Map())")
//     val kont3 = kont2.pop
//     assertEquals(kont3.toString(), "List()")
//   }
// }

// object CESKTests {
//   val invalidCases = List(
//     (
//       "((def foo 123.4) (def foo 0.0) (foo / foo))",
//       CleanProgram(
//         decls = List(
//           CleanDecl(CleanExpr.Var("foo"), CleanExpr.Num(123.4)),
//           CleanDecl(CleanExpr.Var("foo"), CleanExpr.Num(0.0))
//         ),
//         stmts = List(),
//         expr = CleanExpr.BinOpExpr(CleanExpr.Var("foo"), BinOp.Div, CleanExpr.Var("foo"))
//       ),
//       RuntimeError.DivisionByZero(f"Dividing by Zero: 0.0 / 0.0")
//     ),
//     (
//       "((def foo 123.4) (def bar 0.0) (if0 bar (bar = foo / bar) (bar = bar + foo)) bar)",
//       CleanProgram(
//          decls = List(
//           CleanDecl(CleanExpr.Var("foo"), CleanExpr.Num(123.4)),
//           CleanDecl(CleanExpr.Var("bar"), CleanExpr.Num(0.0))
//         ),
//         stmts = List(
//           CleanStmt.Ifelse(
//             CleanExpr.Var("bar"),
//             CleanBlock.One(
//               CleanStmt.Assign(
//                 CleanExpr.Var("bar"), 
//                 CleanExpr.BinOpExpr(CleanExpr.Var("foo"), BinOp.Div, CleanExpr.Var("bar"))
//               )
//             ),
//             CleanBlock.One(
//               CleanStmt.Assign(
//                 CleanExpr.Var("bar"),
//                 CleanExpr.BinOpExpr(CleanExpr.Var("bar"), BinOp.Add, CleanExpr.Var("foo"))
//               )
//             )
//           )
//         ),
//         expr = CleanExpr.Var("bar")
//       ),
//       RuntimeError.DivisionByZero(f"Dividing by Zero: 123.4 / 0.0")
//     ),
//     (
//       "((def foo 123.4) (def bar 0.0) (while0 bar (bar = foo / bar)) bar)",
//       CleanProgram(
//          decls = List(
//           CleanDecl(CleanExpr.Var("foo"), CleanExpr.Num(123.4)),
//           CleanDecl(CleanExpr.Var("bar"), CleanExpr.Num(0.0))
//         ),
//         stmts = List(
//           CleanStmt.While(
//             CleanExpr.Var("bar"),
//             CleanBlock.One(
//               CleanStmt.Assign(
//                 CleanExpr.Var("bar"), 
//                 CleanExpr.BinOpExpr(CleanExpr.Var("foo"), BinOp.Div, CleanExpr.Var("bar"))
//               )
//             ),
//           )
//         ),
//         expr = CleanExpr.Var("bar")
//       ),
//       RuntimeError.DivisionByZero(f"Dividing by Zero: 123.4 / 0.0")
//     ),
//   )
// }
