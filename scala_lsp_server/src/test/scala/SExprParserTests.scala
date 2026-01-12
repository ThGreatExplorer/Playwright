package test

import munit.FunSuite
import sexprs.{Parser, Position, Range}
import sexprs.SExprs._

class SExprParserTests extends FunSuite {
  test("Parser should set range for atom") {
    val parser = Parser.fromString("foo")
    val expr = parser.parse
    assertEquals(expr, SSymbol("foo"))
    assertEquals(expr.getRange, Range(Position(1, 1), Position(1, 4)))
  }

  test("Parser should set range for list spanning tokens") {
    val parser = Parser.fromString("(foo 42)")
    val expr = parser.parse
    assertEquals(expr, SList(List(SSymbol("foo"), SInt(42))))
    assertEquals(expr.getRange, Range(Position(1, 1), Position(1, 9)))
  }

  test("Parser should set range for nested list") {
    val parser = Parser.fromString("(foo (bar))")
    val expr = parser.parse
    assertEquals(expr, SList(List(SSymbol("foo"), SList(List(SSymbol("bar"))))))
    assertEquals(expr.getRange, Range(Position(1, 1), Position(1, 12)))
  }
}
