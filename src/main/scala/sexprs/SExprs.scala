package sexprs

import annotation.tailrec

object SExprs {

  sealed trait SExpr extends Positioned

  case class SList(sexprs: List[SExpr]) extends SExpr
  object SList {
    def apply(sexprs: SExpr*): SList = SList(List(sexprs*))
  }

  case class SInt(n: BigInt)                                  extends SExpr
  case class SDouble(n: Double)                               extends SExpr
  case class SString(s: String)                               extends SExpr
  case class SBoolean(v: Boolean)                             extends SExpr
  case class SSymbol(s: String)                               extends SExpr
  case class SQualifiedSymbol(q: Option[SSymbol], s: SSymbol) extends SExpr

  /* SComment is never parsed, only used for pretty printing */
  case class SComment(s: String) extends SExpr

  /** 
   * Counts number of aNames in a given Example s-expression
   * Throws an exception if an unexpected SExpr node is found 
   * Tail-call optimized to handle deep nesting
   *
   * For Homework 1
   * 
   * @param input SExpr read from stdin
   * @return integer count of aName elements
   */
  def counter(input: SExpr): Int =
    @tailrec
    def loop(stack: List[SExpr], acc: Int): Int = 
    stack match
        case Nil => acc
        case SSymbol(_) :: rest => loop(rest, acc + 1)
        case SDouble(_) :: rest => loop(rest, acc)
        case SList(elements) :: rest => loop(elements ++ rest, acc)
        case head :: _ => throw new Exception("SExpr not part of Example Structure: " + head)
    loop(List(input), 0)
}
