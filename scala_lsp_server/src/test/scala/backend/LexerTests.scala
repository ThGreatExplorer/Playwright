// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html

package test.backend

import test.backend.AstRangeDefaults.*

import munit.FunSuite
import sexprs.Tokens._
import sexprs.{Lexer, Position, Range}

class MySuite extends FunSuite {
  test("Lexer should tokenize parentheses") {
    val lexer = new Lexer(new java.io.StringReader("( )"))
    val open = lexer.nextToken
    assertEquals(open, OParen())
    assertEquals(open.getRange, Range(Position(1, 1), Position(1, 2)))
    val close = lexer.nextToken
    assertEquals(close, CParen())
    assertEquals(close.getRange, Range(Position(1, 3), Position(1, 4)))
    assertEquals(lexer.nextToken, null)
  }

  test("Lexer should tokenize integer") {
    val lexer = new Lexer(new java.io.StringReader("42"))
    val intTok = lexer.nextToken
    assertEquals(intTok, IntLit(42))
    assertEquals(intTok.getRange, Range(Position(1, 1), Position(1, 3)))
    assertEquals(lexer.nextToken, null)
  }

  test("Lexer should tokenize negative integer") {
    val lexer = new Lexer(new java.io.StringReader("-42"))
    val intTok = lexer.nextToken
    assertEquals(intTok, IntLit(-42))
    assertEquals(intTok.getRange, Range(Position(1, 1), Position(1, 4)))
    assertEquals(lexer.nextToken, null)
  }

  test("Lexer should tokenize double without whole part") {
    val lexer = new Lexer(new java.io.StringReader(".4"))
    val dblTok = lexer.nextToken
    assertEquals(dblTok, DoubleLit(0.4))
    assertEquals(dblTok.getRange, Range(Position(1, 1), Position(1, 3)))
    assertEquals(lexer.nextToken, null)
  }

  test("Lexer should tokenize negative double without whole part") {
    val lexer = new Lexer(new java.io.StringReader("-.4"))
    val dblTok = lexer.nextToken
    assertEquals(dblTok, DoubleLit(-0.4))
    assertEquals(dblTok.getRange, Range(Position(1, 1), Position(1, 4)))
    assertEquals(lexer.nextToken, null)
  }

  test("Lexer should tokenize double") {
    val lexer = new Lexer(new java.io.StringReader("3.4"))
    val dblTok = lexer.nextToken
    assertEquals(dblTok, DoubleLit(3.4))
    assertEquals(dblTok.getRange, Range(Position(1, 1), Position(1, 4)))
    assertEquals(lexer.nextToken, null)
  }

  test("Lexer should tokenize negative double") {
    val lexer = new Lexer(new java.io.StringReader("-3.4"))
    val dblTok = lexer.nextToken
    assertEquals(dblTok, DoubleLit(-3.4))
    assertEquals(dblTok.getRange, Range(Position(1, 1), Position(1, 5)))
    assertEquals(lexer.nextToken, null)
  }

  test("Lexer handles - as symbol") {
    val lexer = new Lexer(new java.io.StringReader("-abc"))
    val sym = lexer.nextToken
    assertEquals(sym, SymbolLit("-abc"))
    assertEquals(sym.getRange, Range(Position(1, 1), Position(1, 5)))
    assertEquals(lexer.nextToken, null)

    val lexer2 = new Lexer(new java.io.StringReader("- abc"))
    val sym2 = lexer2.nextToken
    assertEquals(sym2, SymbolLit("-"))
    assertEquals(sym2.getRange, Range(Position(1, 1), Position(1, 2)))
    val sym3 = lexer2.nextToken
    assertEquals(sym3, SymbolLit("abc"))
    assertEquals(sym3.getRange, Range(Position(1, 3), Position(1, 6)))
    assertEquals(lexer2.nextToken, null)

    val lexer3 = new Lexer(new java.io.StringReader("--42"))
    val sym4 = lexer3.nextToken
    assertEquals(sym4, SymbolLit("--42"))
    assertEquals(sym4.getRange, Range(Position(1, 1), Position(1, 5)))
    assertEquals(lexer3.nextToken, null)
  }

  test("Lexer should tokenize symbol") {
    val lexer = new Lexer(new java.io.StringReader("foo"))
    val sym = lexer.nextToken
    assertEquals(sym, SymbolLit("foo"))
    assertEquals(sym.getRange, Range(Position(1, 1), Position(1, 4)))
    assertEquals(lexer.nextToken, null)
  }

  test("Lexer should tokenize string literal") {
    val lexer = new Lexer(new java.io.StringReader("\"bar\""))
    val str = lexer.nextToken
    assertEquals(str, StringLit("bar"))
    // end is exclusive, quotes are part of the literal
    assertEquals(str.getRange, Range(Position(1, 1), Position(1, 6)))
    assertEquals(lexer.nextToken, null)
  }

  test("Lexer should track range across lines") {
    val lexer = new Lexer(new java.io.StringReader("\nfoo\n"))
    val sym = lexer.nextToken
    assertEquals(sym, SymbolLit("foo"))
    assertEquals(sym.getRange, Range(Position(2, 1), Position(2, 4)))
    assertEquals(lexer.nextToken, null)
  }
}
