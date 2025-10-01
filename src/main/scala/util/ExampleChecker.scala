package util

import sexprs.SExprs._
import scala.annotation.tailrec

object ExampleKeyword:
  val Def = "def"
  val Assign = "="
  val If = "if0"
  val While = "while0"
  val Block = "block"
  val Div = "/"
  val Plus = "+"
  val Eq = "=="

  private val allKeywords: Set[String] = Set(
    Def, Assign, If, While, Block, Div, Plus, Eq
  )
  
  def isKeyword(s: String): Boolean = allKeywords.contains(s)

object ExampleChecker:
  /** Internal utility method to assert that a given SExpr is an ExampleCC
   *  Useful to ensring the unit tests we write respect the spec
   * 
   * @param input SExpr read from stdio
  */
  def assertExample(input :SExpr): Unit = 
    input match 
      case SSymbol(x) => {
        val isSimpleName = (x.length() <= 20 && x.forall(_.isLetter))
        val isBBName = isSimpleName || ExampleKeyword.isKeyword(x)
        if (!isBBName)
          throw new InputNotExampleException("SExpr contains SSymbol that is not an ExampleBB Name: " + x)
      }        
      case SDouble(n) => {
        val isGoodNum = (n >= -1000.0) && (n <= 1000.0)
        if (!isGoodNum)
          throw new InputNotExampleException("SExpr contains SDouble that is not an ExampleBB Number: " + n)
      }
      case SList(elements) => elements.map(assertExample)
      case _ => 
        throw new InputNotExampleException("SExpr not part of Example Structure: " + input)
