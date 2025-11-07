package test

import main.MainFuncs
import static._
import ast._
import munit.FunSuite
import ast.ConverterToClean.systemToClean
import ast.ProgBlock

class TypecheckerTests extends FunSuite {

  TypecheckerTests.testCases.zip(TypecheckerTests.expectedTestCaseResults)foreach{ 
    case (inputStr, expectedProg) =>
      test(inputStr) {
        val inputSexp = MainFuncs.readSexp(inputStr)  
        val pipeRes =         
          for 
            parsedProg <- systemToClean(Parser.parseTypedSys(inputSexp))
            vCheck1    <- systemToClean(VCheckTLDups.moduleDupsSys(parsedProg))
            vCheck2    <- systemToClean(VCheckMFPNameDups.mfpDupsSys(vCheck1))                
            validPr    <- systemToClean(VCheckUndefined.closedSystem(vCheck2))
          yield
            validPr

        pipeRes match
          case None => throw new Exception("Passed invalid test case for Typechecker")
          case Some(cleanProg) =>
            val progWE = Typechecker.typecheckSystem(cleanProg)
            assertEquals(progWE, expectedProg)
      }  
  }
}

object TypecheckerTests {

val testCases = List(
  """
  ((tmodule
  Point
  (class Point (x y) (method delta (x) (def y (this --> y)) (x = 1.0) (x + y)))
  (((x Number) (y Number)) ((delta (Number) Number))))
 (import Point)
 (def x 1.0)
 (def point (new Point (x x)))
 (point --> x = x)
 (x = (point --> delta (x)))
 x)
  """,
  """
  ((tmodule
  Point
  (class Point (x y) (method delta (x) (def y (this --> y)) (x = 1.0) (x + y)))
  (((x Number) (y Number)) ((delta (Number) Number))))
 (tmodule
  PointTwo
  (class Point
    (x y z)
    (method delta () (def x (this --> x)) (def y (this --> y)) (x + y)))
  (((x Number) (y Number))
   ((delta (Number) (((x Number) (y Number)) ((delta (Number) Number)))))))
 (import Point)
 (def x 1.0)
 (def point (new Point (x x)))
 (point --> x = x)
 (x = (point --> delta (x)))
 x)
  """,
  """
  ((tmodule
  PointThreeD
  (class PointThreeD
    (x y z)
    (method delta () (def x (this --> x)) (def y (this --> y)) (x + y)))
  (((x Number) (z Number) (y Number)) ((delta (Number) Number))))
 (import PointThreeD)
 (def x 1.0)
 (def point (new PointThreeD (x x x)))
 (point --> x = x)
 (x = (point --> delta (x)))
 x)
  """,
  """
  ((tmodule
  PointThreeD
  (class PointThreeD
    (x y z)
    (method delta () (def x (this --> x)) (def y (this --> y)) (x + y)))
  (((x Number) (y Number) (z Number)) ((delta () Number))))
 (import PointThreeD)
 (def x 1.0)
 (def point (new PointThreeD (x x x)))
 (point --> x = x)
 (x = (point --> deltaOne (x)))
 x)
  """,
  """
  ((tmodule
  PointThreeD
  (class PointThreeD
    (x y z)
    (method delta () (def x (this --> x)) (def y (this --> y)) (x + y)))
  (((x Number) (y Number) (z Number)) ((delta () Number))))
 (import PointThreeD)
 (def x 1.0)
 (def point (new PointThreeD (x x x)))
 (point --> x = x)
 (x = (point --> delta ()))
 x)
  """,
  """
 ((tmodule
  PointThreeD
  (class PointThreeD
    (x y z)
    (method delta () (def x (this --> x)) (def y (this --> y)) (x + y)))
  (((x Number) (y Number) (z Number)) ((delta () Number) (deltaOne () Number))))
 (import PointThreeD) 
 (def x 1.0)
 (def point (new PointThreeD (x x x)))
 (x = (point --> delta (x x)))
 (x = (point --> m))
 (x = (x --> x))
 (x = (x isa PointThreeD))
 x)
""",
"""
( 
  (tmodule Mult (class Multiplier ()
    (method times (m n)
      (def negOne -1.0)
      (def result 0.0) 
      (def keepRunning 0.0)

      (while0 keepRunning
        (block 
          (result = (result + m))
          (n = (n + negOne))
          (if0 n
            (keepRunning = 1.0)
            (keepRunning = 0.0))
          ))
      result))
    (() ((times (Number Number) Number))))

  (tmodule Fact (import Mult) 
    (class Fact ()
      (method calcN (n)
        (def result 1.0)
        (if0 n
          (result = 1.0)
          (block
            (def negOne -1.0)
            (def nMinOne (n + negOne))
            (def calcNminOne (this --> calcN (nMinOne)))

            (def multiplier (new Multiplier ()))
            (result = (multiplier --> times (n calcNminOne)))
          ))
        result))
      (() ((calcN (Number) Number))))

  (import Fact)
  
  (def factorial (new Fact ()))
  (def n 5.0)
  (factorial --> calcN (n))
)
"""
 )

val expectedTestCaseResults = List(
  WE.Node(System(
    List(
      WE.Node(Module(
        WE.Node("Point"),
        List(),
        WE.Node(Class(
          WE.Node("Point"),
          List(WE.Node("x"), WE.Node("y")),
          List(
            WE.Node(Method(
              WE.Node("delta"),
              List(WE.Node("x")),
              WE.Node(ProgBlock(
                List(
                  WE.Node(Decl(
                    WE.Node("y"),
                    WE.Node(Expr.GetField(
                      WE.Node("this"),
                      WE.Node("y")
                    ))
                  ))
                ),
                List(
                  WE.Node(Stmt.Assign(
                    WE.Node("x"),
                    WE.Node(Expr.Num(1.0))
                  ))
                ),
                WE.Node(Expr.BinOpExpr(
                  WE.Node("x"),
                  BinOp.Add,
                  WE.Node("y")
                ))
              ))
            ))
          )
        )),
        Some(WE.Node(Type.Shape(
          List(
            WE.Node(FieldType(
              WE.Node("x"),
              WE.Node(Type.Number())
            )),
            WE.Node(FieldType(
              WE.Node("y"),
              WE.Node(Type.Number())
            ))
          ),
          List(
            WE.Node(MethodType(
              WE.Node("delta"),
              List(WE.Node(Type.Number())),
              WE.Node(Type.Number())
            ))
          )
        )))
      ))
    ),
    List(WE.Node("Point")),
    WE.Node(ProgBlock(
      List(
        WE.Node(Decl(
          WE.Node("x"),
          WE.Node(Expr.Num(1.0))
        )),
        WE.Node(Decl(
          WE.Node("point"),
          WE.Node(Expr.NewInstance(
            WE.Node("Point"),
            List(WE.Node("x"), WE.Node("x"))
          ))
        ))
      ),
      List(
        WE.Node(Stmt.FieldAssign(
          WE.Node("point"),
          WE.Node("x"),
          WE.Node(Expr.Var(WE.Node("x")))
        )),
        WE.Node(Stmt.Assign(
          WE.Node("x"),
          WE.Node(Expr.CallMethod(
            WE.Node("point"),
            WE.Node("delta"),
            List(WE.Node("x"))
          ))
        ))
      ),
      WE.Node(Expr.Var(WE.Node("x")))
    ))
  )),
  WE.Node(System(
    List(
      WE.Node(Module(
        WE.Node("Point"),
        List(),
        WE.Node(Class(
          WE.Node("Point"),
          List(WE.Node("x"), WE.Node("y")),
          List(
            WE.Node(Method(
              WE.Node("delta"),
              List(WE.Node("x")),
              WE.Node(ProgBlock(
                List(
                  WE.Node(Decl(
                    WE.Node("y"),
                    WE.Node(Expr.GetField(
                      WE.Node("this"),
                      WE.Node("y")
                    ))
                  ))
                ),
                List(
                  WE.Node(Stmt.Assign(
                    WE.Node("x"),
                    WE.Node(Expr.Num(1.0))
                  ))
                ),
                WE.Node(Expr.BinOpExpr(
                  WE.Node("x"),
                  BinOp.Add,
                  WE.Node("y")
                ))
              ))
            ))
          )
        )),
        Some(WE.Node(Type.Shape(
          List(
            WE.Node(FieldType(
              WE.Node("x"),
              WE.Node(Type.Number())
            )),
            WE.Node(FieldType(
              WE.Node("y"),
              WE.Node(Type.Number())
            ))
          ),
          List(
            WE.Node(MethodType(
              WE.Node("delta"),
              List(WE.Node(Type.Number())),
              WE.Node(Type.Number())
            ))
          )
        )))
      )),
      WE.Node(Module(
        WE.Node("PointTwo"),
        List(),
        WE.Err(TypeErrorNodes.ShapeTypeWrongNumberOfFields),
        Some(WE.Node(Type.Shape(
          List(
            WE.Node(FieldType(
              WE.Node("x"),
              WE.Node(Type.Number())
            )),
            WE.Node(FieldType(
              WE.Node("y"),
              WE.Node(Type.Number())
            ))
          ),
          List(
            WE.Node(MethodType(
              WE.Node("delta"),
              List(WE.Node(Type.Number())),
              WE.Node(Type.Shape(
                List(
                  WE.Node(FieldType(
                    WE.Node("x"),
                    WE.Node(Type.Number())
                  )),
                  WE.Node(FieldType(
                    WE.Node("y"),
                    WE.Node(Type.Number())
                  ))
                ),
                List(
                  WE.Node(MethodType(
                    WE.Node("delta"),
                    List(WE.Node(Type.Number())),
                    WE.Node(Type.Number())
                  ))
                )
              ))
            ))
          )
        )))
      ))
    ),
    List(WE.Node("Point")),
    WE.Node(ProgBlock(
      List(
        WE.Node(Decl(
          WE.Node("x"),
          WE.Node(Expr.Num(1.0))
        )),
        WE.Node(Decl(
          WE.Node("point"),
          WE.Node(Expr.NewInstance(
            WE.Node("Point"),
            List(WE.Node("x"), WE.Node("x"))
          ))
        ))
      ),
      List(
        WE.Node(Stmt.FieldAssign(
          WE.Node("point"),
          WE.Node("x"),
          WE.Node(Expr.Var(WE.Node("x")))
        )),
        WE.Node(Stmt.Assign(
          WE.Node("x"),
          WE.Node(Expr.CallMethod(
            WE.Node("point"),
            WE.Node("delta"),
            List(WE.Node("x"))
          ))
        ))
      ),
      WE.Node(Expr.Var(WE.Node("x")))
    ))
  )),
  WE.Node(System(
    List(
      WE.Node(Module(
        WE.Node("PointThreeD"),
        List(),
        WE.Node(Class(
          WE.Node("PointThreeD"),
          List(
            WE.Node("x"),
            WE.Err(TypeErrorNodes.ShapeTypeFieldTypeMismatch),
            WE.Err(TypeErrorNodes.ShapeTypeFieldTypeMismatch)
          ),
          List(
            WE.Err(TypeErrorNodes.ShapeTypeMethodWrongNumberOfParams)
          )
        )),
        Some(WE.Node(Type.Shape(
          List(
            WE.Node(FieldType(
              WE.Node("x"),
              WE.Node(Type.Number())
            )),
            WE.Node(FieldType(
              WE.Node("z"),
              WE.Node(Type.Number())
            )),
            WE.Node(FieldType(
              WE.Node("y"),
              WE.Node(Type.Number())
            ))
          ),
          List(
            WE.Node(MethodType(
              WE.Node("delta"),
              List(WE.Node(Type.Number())),
              WE.Node(Type.Number())
            ))
          )
        )))
      ))
    ),
    List(WE.Node("PointThreeD")),
    WE.Node(ProgBlock(
      List(
        WE.Node(Decl(
          WE.Node("x"),
          WE.Node(Expr.Num(1.0))
        )),
        WE.Node(Decl(
          WE.Node("point"),
          WE.Node(Expr.NewInstance(
            WE.Node("PointThreeD"),
            List(WE.Node("x"), WE.Node("x"), WE.Node("x"))
          ))
        ))
      ),
      List(
        WE.Node(Stmt.FieldAssign(
          WE.Node("point"),
          WE.Node("x"),
          WE.Node(Expr.Var(WE.Node("x")))
        )),
        WE.Node(Stmt.Assign(
          WE.Node("x"),
          WE.Node(Expr.CallMethod(
            WE.Node("point"),
            WE.Node("delta"),
            List(WE.Node("x"))
          ))
        ))
      ),
      WE.Node(Expr.Var(WE.Node("x")))
    ))
  )),
  // ...existing code...
WE.Node(System(
  List(
    WE.Node(Module(
      WE.Node("PointThreeD"),
      List(),
      WE.Node(Class(
        WE.Node("PointThreeD"),
        List(WE.Node("x"), WE.Node("y"), WE.Node("z")),
        List(
          WE.Node(Method(
            WE.Node("delta"),
            List(),
            WE.Node(ProgBlock(
              List(
                WE.Node(Decl(
                  WE.Node("x"),
                  WE.Node(Expr.GetField(
                    WE.Node("this"),
                    WE.Node("x")
                  ))
                )),
                WE.Node(Decl(
                  WE.Node("y"),
                  WE.Node(Expr.GetField(
                    WE.Node("this"),
                    WE.Node("y")
                  ))
                ))
              ),
              List(),
              WE.Node(Expr.BinOpExpr(
                WE.Node("x"),
                BinOp.Add,
                WE.Node("y")
              ))
            ))
          ))
        )
      )),
      Some(WE.Node(Type.Shape(
        List(
          WE.Node(FieldType(
            WE.Node("x"),
            WE.Node(Type.Number())
          )),
          WE.Node(FieldType(
            WE.Node("y"),
            WE.Node(Type.Number())
          )),
          WE.Node(FieldType(
            WE.Node("z"),
            WE.Node(Type.Number())
          ))
        ),
        List(
          WE.Node(MethodType(
            WE.Node("delta"),
            List(),
            WE.Node(Type.Number())
          ))
        )
      )))
    ))
  ),
  List(WE.Node("PointThreeD")),
  WE.Node(ProgBlock(
    List(
      WE.Node(Decl(
        WE.Node("x"),
        WE.Node(Expr.Num(1.0))
      )),
      WE.Node(Decl(
        WE.Node("point"),
        WE.Node(Expr.NewInstance(
          WE.Node("PointThreeD"),
          List(WE.Node("x"), WE.Node("x"), WE.Node("x"))
        ))
      ))
    ),
    List(
      WE.Node(Stmt.FieldAssign(
        WE.Node("point"),
        WE.Node("x"),
        WE.Node(Expr.Var(WE.Node("x")))
      )),
      WE.Node(Stmt.Assign(
        WE.Node("x"),
        WE.Node(Expr.CallMethod(
          WE.Node("point"),
          WE.Err(TypeErrorNodes.CallMethodDoesNotExist),
          List(WE.Node("x"))
        ))
      ))
    ),
    WE.Node(Expr.Var(WE.Node("x")))
  ))
)),
  WE.Node(System(
  List(
    WE.Node(Module(
      WE.Node("PointThreeD"),
      List(),
      WE.Node(Class(
        WE.Node("PointThreeD"),
        List(WE.Node("x"), WE.Node("y"), WE.Node("z")),
        List(
          WE.Node(Method(
            WE.Node("delta"),
            List(),
            WE.Node(ProgBlock(
              List(
                WE.Node(Decl(
                  WE.Node("x"),
                  WE.Node(Expr.GetField(
                    WE.Node("this"),
                    WE.Node("x")
                  ))
                )),
                WE.Node(Decl(
                  WE.Node("y"),
                  WE.Node(Expr.GetField(
                    WE.Node("this"),
                    WE.Node("y")
                  ))
                ))
              ),
              List(),
              WE.Node(Expr.BinOpExpr(
                WE.Node("x"),
                BinOp.Add,
                WE.Node("y")
              ))
            ))
          ))
        )
      )),
      Some(WE.Node(Type.Shape(
        List(
          WE.Node(FieldType(
            WE.Node("x"),
            WE.Node(Type.Number())
          )),
          WE.Node(FieldType(
            WE.Node("y"),
            WE.Node(Type.Number())
          )),
          WE.Node(FieldType(
            WE.Node("z"),
            WE.Node(Type.Number())
          ))
        ),
        List(
          WE.Node(MethodType(
            WE.Node("delta"),
            List(),
            WE.Node(Type.Number())
          ))
        )
      )))
    ))
  ),
  List(WE.Node("PointThreeD")),
  WE.Node(ProgBlock(
    List(
      WE.Node(Decl(
        WE.Node("x"),
        WE.Node(Expr.Num(1.0))
      )),
      WE.Node(Decl(
        WE.Node("point"),
        WE.Node(Expr.NewInstance(
          WE.Node("PointThreeD"),
          List(WE.Node("x"), WE.Node("x"), WE.Node("x"))
        ))
      ))
    ),
    List(
      WE.Node(Stmt.FieldAssign(
        WE.Node("point"),
        WE.Node("x"),
        WE.Node(Expr.Var(WE.Node("x")))
      )),
      WE.Node(Stmt.Assign(
        WE.Node("x"),
        WE.Node(Expr.CallMethod(
          WE.Node("point"),
          WE.Node("delta"),
          List()
        ))
      ))
    ),
    WE.Node(Expr.Var(WE.Node("x")))
  ))
)),
WE.Node(System(
  List(
    WE.Node(Module(
      WE.Node("PointThreeD"),
      List(),
      WE.Err(TypeErrorNodes.ShapeTypeWrongNumberOfMethods),
      Some(WE.Node(Type.Shape(
        List(
          WE.Node(FieldType(
            WE.Node("x"),
            WE.Node(Type.Number())
          )),
          WE.Node(FieldType(
            WE.Node("y"),
            WE.Node(Type.Number())
          )),
          WE.Node(FieldType(
            WE.Node("z"),
            WE.Node(Type.Number())
          ))
        ),
        List(
          WE.Node(MethodType(
            WE.Node("delta"),
            List(),
            WE.Node(Type.Number())
          )),
          WE.Node(MethodType(
            WE.Node("deltaOne"),
            List(),
            WE.Node(Type.Number())
          ))
        )
      )))
    ))
  ),
  List(WE.Node("PointThreeD")),
  WE.Node(ProgBlock(
    List(
      WE.Node(Decl(
        WE.Node("x"),
        WE.Node(Expr.Num(1.0))
      )),
      WE.Node(Decl(
        WE.Node("point"),
        WE.Node(Expr.NewInstance(
          WE.Node("PointThreeD"),
          List(WE.Node("x"), WE.Node("x"), WE.Node("x"))
        ))
      ))
    ),
    List(
      WE.Node(Stmt.Assign(
        WE.Node("x"),
        WE.Err(TypeErrorNodes.CallMethodWrongNumberOfParams)
      )),
      WE.Node(Stmt.Assign(
        WE.Node("x"),
        WE.Node(Expr.GetField(
          WE.Node("point"),
          WE.Err(TypeErrorNodes.FieldDoesNotExist)
        ))
      )),
      WE.Node(Stmt.Assign(
        WE.Node("x"),
        WE.Err(TypeErrorNodes.GetFieldCalledOnNonShapeType)
      )),
      WE.Node(Stmt.Assign(
        WE.Node("x"),
        WE.Err(TypeErrorNodes.IsACalledWithNonNumberType)
      ))
    ),
    WE.Node(Expr.Var(WE.Node("x")))
  ))
)),
WE.Node(System(
  List(
    WE.Node(Module(
      WE.Node("Mult"),
      List(),
      WE.Node(Class(
        WE.Node("Multiplier"),
        List(),
        List(
          WE.Node(Method(
            WE.Node("times"),
            List(WE.Node("m"), WE.Node("n")),
            WE.Node(ProgBlock(
              List(
                WE.Node(Decl(
                  WE.Node("negOne"),
                  WE.Node(Expr.Num(-1.0))
                )),
                WE.Node(Decl(
                  WE.Node("result"),
                  WE.Node(Expr.Num(0.0))
                )),
                WE.Node(Decl(
                  WE.Node("keepRunning"),
                  WE.Node(Expr.Num(0.0))
                ))
              ),
              List(
                WE.Node(Stmt.While(
                  WE.Node(Expr.Var(WE.Node("keepRunning"))),
                  WE.Node(StmtBlock.Many(
                    List(),
                    List(
                      WE.Node(Stmt.Assign(
                        WE.Node("result"),
                        WE.Node(Expr.BinOpExpr(
                          WE.Node("result"),
                          BinOp.Add,
                          WE.Node("m")
                        ))
                      )),
                      WE.Node(Stmt.Assign(
                        WE.Node("n"),
                        WE.Node(Expr.BinOpExpr(
                          WE.Node("n"),
                          BinOp.Add,
                          WE.Node("negOne")
                        ))
                      )),
                      WE.Node(Stmt.Ifelse(
                        WE.Node(Expr.Var(WE.Node("n"))),
                        WE.Node(StmtBlock.One(
                          WE.Node(Stmt.Assign(
                            WE.Node("keepRunning"),
                            WE.Node(Expr.Num(1.0))
                          ))
                        )),
                        WE.Node(StmtBlock.One(
                          WE.Node(Stmt.Assign(
                            WE.Node("keepRunning"),
                            WE.Node(Expr.Num(0.0))
                          ))
                        ))
                      ))
                    )
                  ))
                ))
              ),
              WE.Node(Expr.Var(WE.Node("result")))
            ))
          ))
        )
      )),
      Some(WE.Node(Type.Shape(
        List(),
        List(
          WE.Node(MethodType(
            WE.Node("times"),
            List(WE.Node(Type.Number()), WE.Node(Type.Number())),
            WE.Node(Type.Number())
          ))
        )
      )))
    )),
    WE.Node(Module(
      WE.Node("Fact"),
      List(WE.Node("Mult")),
      WE.Node(Class(
        WE.Node("Fact"),
        List(),
        List(
          WE.Node(Method(
            WE.Node("calcN"),
            List(WE.Node("n")),
            WE.Node(ProgBlock(
              List(
                WE.Node(Decl(
                  WE.Node("result"),
                  WE.Node(Expr.Num(1.0))
                ))
              ),
              List(
                WE.Node(Stmt.Ifelse(
                  WE.Node(Expr.Var(WE.Node("n"))),
                  WE.Node(StmtBlock.One(
                    WE.Node(Stmt.Assign(
                      WE.Node("result"),
                      WE.Node(Expr.Num(1.0))
                    ))
                  )),
                  WE.Node(StmtBlock.Many(
                    List(
                      WE.Node(Decl(
                        WE.Node("negOne"),
                        WE.Node(Expr.Num(-1.0))
                      )),
                      WE.Node(Decl(
                        WE.Node("nMinOne"),
                        WE.Node(Expr.BinOpExpr(
                          WE.Node("n"),
                          BinOp.Add,
                          WE.Node("negOne")
                        ))
                      )),
                      WE.Node(Decl(
                        WE.Node("calcNminOne"),
                        WE.Node(Expr.CallMethod(
                          WE.Node("this"),
                          WE.Node("calcN"),
                          List(WE.Node("nMinOne"))
                        ))
                      )),
                      WE.Node(Decl(
                        WE.Node("multiplier"),
                        WE.Node(Expr.NewInstance(
                          WE.Node("Multiplier"),
                          List()
                        ))
                      ))
                    ),
                    List(
                      WE.Node(Stmt.Assign(
                        WE.Node("result"),
                        WE.Node(Expr.CallMethod(
                          WE.Node("multiplier"),
                          WE.Node("times"),
                          List(WE.Node("n"), WE.Node("calcNminOne"))
                        ))
                      ))
                    )
                  ))
                ))
              ),
              WE.Node(Expr.Var(WE.Node("result")))
            ))
          ))
        )
      )),
      Some(WE.Node(Type.Shape(
        List(),
        List(
          WE.Node(MethodType(
            WE.Node("calcN"),
            List(WE.Node(Type.Number())),
            WE.Node(Type.Number())
          ))
        )
      )))
    ))
  ),
  List(WE.Node("Fact")),
  WE.Node(ProgBlock(
    List(
      WE.Node(Decl(
        WE.Node("factorial"),
        WE.Node(Expr.NewInstance(
          WE.Node("Fact"),
          List()
        ))
      )),
      WE.Node(Decl(
        WE.Node("n"),
        WE.Node(Expr.Num(5.0))
      ))
    ),
    List(),
    WE.Node(Expr.CallMethod(
      WE.Node("factorial"),
      WE.Node("calcN"),
      List(WE.Node("n"))
    ))
  ))
))    
)

  // val expectedTestCaseResults = List(
  //   WE.Node(System(
  //     List(),
  //     List(),
  //     WE.Node(ProgBlock(
  //       List(),
  //       List(),
  //       WE.Node(Expr.Num(0.0))
  //     ))
  //   )),
  //    WE.Node(System(
  //     List(),
  //     List(),
  //     WE.Node(ProgBlock(
  //       List(),
  //       List(),
  //       WE.Node(Expr.Num(0.0))
  //     ))
  //   ))
  //   ,
  //    WE.Node(System(
  //     List(),
  //     List(),
  //     WE.Node(ProgBlock(
  //       List(),
  //       List(),
  //       WE.Node(Expr.Num(0.0))
  //     ))
  //   ))
  // )

}