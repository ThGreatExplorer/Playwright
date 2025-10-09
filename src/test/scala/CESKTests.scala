// package test

// import munit.FunSuite
// import main.MainFuncs
// import static.Parser
// import ast._
// import cesk.{CESKMachine, RuntimeError}

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
      
//     )
//   }
// }

// object CESKTests {
//   val invalidCases = Seq(
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
    
//   )
// }
