
// From https://github.com/regb/scala-sexprs?? How to attritribute idk
import sexprs.Lexer
import sexprs.Parser
import sexprs.SExprs._

import java.io.StringReader

import scala.io.StdIn.readLine

@main def hello(): Unit =
  println("Hello world!")

  val reader1 = new StringReader("""
    (test "test")
  """)
  // val reader1 = new StringReader(readLine())
  val lexer1 = new Lexer(reader1)
  val parser1 = new Parser(lexer1)
  val parsed1 = parser1.parse
  assert(parsed1 == SList(List(SSymbol("test"), SString("test"))))


  val reader2 = new StringReader("""
    (  42  42.173 )
  """)
  val lexer2 = new Lexer(reader2)
  val parser2 = new Parser(lexer2)
  val parsed2 = parser2.parse
  assert(parsed2 == SList(List(SInt(42), SDouble(42.173))))

  val reader3 = new StringReader("""
    (a 1.0 c)
  """)
  val lexer3 = new Lexer(reader3)
  val parser3 = new Parser(lexer3)
  val parsed3 = parser3.parse
  assert(parsed3 == SList(List(SSymbol("a"), SDouble(1.0), SSymbol("c"))))

  println(msg)

def msg = "All parsed well!"


enum Example:
  case aName
  case aNumber
  case aSeq(examples :List[Example])