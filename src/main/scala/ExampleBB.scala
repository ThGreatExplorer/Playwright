package ExampleBB

import sexprs.SExprs._
import scala.annotation.tailrec

val bbKeywords: Set[String] = Set("=", "if0", "while0", "block", "/", "+", "==")

object Checker:
  
  /** Internal utility method to assert that a given SExpr is an ExampleBB
   *  Useful to ensring the unit tests we write respect the spec
   * 
   * @param input SExpr read from stdio
  */
  def assertExampleBB(input :SExpr): Unit = 
    input match 
      case SSymbol(x) => {
        val isSimpleName = (x.length() <= 20 && x.forall(_.isLetter))
        val isBBName = isSimpleName || bbKeywords.contains(x)
        if (!isBBName)
          throw new Exception("SExpr contains SSymbol that is not an ExampleBB Name: " + x)
      }        
      case SDouble(n) => {
        val isGoodNum = (n >= -1000.0) && (n <= 1000.0)
        if (!isGoodNum)
            throw new Exception("SExpr contains SDouble that is not an ExampleBB Number: " + n)
      }
      case SList(elements) => {
        @tailrec
        def loop(remaining: List[SExpr]): Unit =
          remaining match
            case Nil => ()
            case h :: t => assertExampleBB(h); loop(t)
        loop(elements)
      } 
      case _ => throw new Exception("SExpr not part of Example Structure: " + input)
