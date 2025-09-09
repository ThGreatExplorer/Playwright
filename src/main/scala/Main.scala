
import sexprs.Lexer
import sexprs.Parser
import sexprs.SExprs._

import java.io.StringReader

import scala.io.StdIn.readLine

def counter(input :SExpr): Int = {
  print("\n" + input)
  input match {
    case SSymbol(_) => 1
    case SDouble(_) => 0
    case SList(elements) => elements.map(counter).sum
    case _ => throw new Exception("SExpr not part of Example Structure: " + input)
  }
}

@main def hello(): Unit =
  println("Hello world!")

  // val reader1 = new StringReader("""
  //   (test "test")
  // """)
  val reader1 = new StringReader(readLine())
  val lexer1 = new Lexer(reader1)
  val parser1 = new Parser(lexer1)
  val parsed1 = parser1.parse
  val nameCount = counter(parsed1)
  println(parsed1)
  println(nameCount)
