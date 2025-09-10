// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html


package sexprs

import munit.FunSuite
import Tokens._

class MySuite extends FunSuite {
  test("Lexer should tokenize parentheses") {
    val lexer = new Lexer(new java.io.StringReader("( )"))
    assertEquals(lexer.nextToken, OParen())
    assertEquals(lexer.nextToken, CParen())
    assertEquals(lexer.nextToken, null)
  }

  test("Lexer should tokenize integer") {
    val lexer = new Lexer(new java.io.StringReader("42"))
    assertEquals(lexer.nextToken, IntLit(42))
    assertEquals(lexer.nextToken, null)
  }

  test("Lexer should tokenize negative integer") {
    val lexer = new Lexer(new java.io.StringReader("-42"))
    assertEquals(lexer.nextToken, IntLit(-42))
    assertEquals(lexer.nextToken, null)
  }

  test("Lexer should tokenize double") {
    val lexer = new Lexer(new java.io.StringReader("3.4"))
    assertEquals(lexer.nextToken, DoubleLit(3.4))
    assertEquals(lexer.nextToken, null)
  }

  test("Lexer should tokenize negative double") {
    val lexer = new Lexer(new java.io.StringReader("-3.4"))
    assertEquals(lexer.nextToken, DoubleLit(-3.4))
    assertEquals(lexer.nextToken, null)
  }

  test("Lexer handles - as symbol") {
    val lexer = new Lexer(new java.io.StringReader("-abc"))
    assertEquals(lexer.nextToken, SymbolLit("-abc"))
    assertEquals(lexer.nextToken, null)

    val lexer2 = new Lexer(new java.io.StringReader("- abc"))
    assertEquals(lexer2.nextToken, SymbolLit("-"))
    assertEquals(lexer2.nextToken, SymbolLit("abc"))
    assertEquals(lexer2.nextToken, null)

    val lexer3 = new Lexer(new java.io.StringReader("--42"))
    assertEquals(lexer3.nextToken, SymbolLit("--42"))
    assertEquals(lexer3.nextToken, null)
  }

  test("Lexer should tokenize symbol") {
    val lexer = new Lexer(new java.io.StringReader("foo"))
    assertEquals(lexer.nextToken, SymbolLit("foo"))
    assertEquals(lexer.nextToken, null)
  }

  test("Lexer should tokenize string literal") {
    val lexer = new Lexer(new java.io.StringReader("\"bar\""))
    assertEquals(lexer.nextToken, StringLit("bar"))
    assertEquals(lexer.nextToken, null)
  }
}