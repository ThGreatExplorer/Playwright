package csk

import ast._
import scala.annotation.static
import scala.collection.mutable.Map

class UnreachableStateException(msg: String) extends Exception(msg)
class UnloadedNonFinalStateException(msg: String) extends Exception(msg)

enum Control:
  case Value(n : Expression)
  case Err(e : RuntimeError)
  case Search

enum Kont:
  case Prog(stmts: List[Statement | Block], expr: Expression)
  case Empty

class CSKMachine

object CSKMachine:

  /**
  * Runs a program to completion, returning either the final number or an error
  *
  * @param prog the program AST
  * @return the final number or an error
  * @throws UnreachableStateException if the program is malformed or an unexpected state is reached
  */
  @static
  def run(prog: Program) : Number | Control.Err =
    var state = CSKMachine.load(prog)
    while !CSKMachine.isFinal(state) do
      state = state.transition()
    CSKMachine.unload(state)

  /**
    * Loads a program into the initial state of the CSK machine where the control is set to Search, the store is empty, and the continuation contains all statements and the final expression in order.
    *
    * @param prog the programAST
    * @return the initial CSKState
    * @throws UnreachableStateException if the program is malformed (i.e. there is a parser error)
    */
  @static
  private def load(prog: Program) : CSKState = 
    prog match
      case Program.Err(e) => throw new UnreachableStateException("Passed a malformed program to the CSK machine")
      case Program.Prog(stmts, expr) =>
        new CSKState(
          control = Control.Search,
          store = Map(),
          kont = Kont.Prog(stmts, expr)
        )

  /**
    * Returns true if the given state is a final state where the final state is either an Error state or a state with a final number and a continuation with only expression left.
    *
    * @param state the current state of the CSK machine
    * @return true if the state is final, false otherwise
    */
  @static
  private def isFinal(state: CSKState) : Boolean =
    (state.control, state.store, state.kont) match { 
      case (Control.Err(_), _, Kont.Empty) => true
      case (Control.Value(Expression.Num(_)), _, Kont.Prog(Nil, expr)) => true
      case _ => false
    }

  
  /**
    * Unloads the final result of a computation from a final CSKState.
    * 
    * @param state a final CSKState
    * @throws UnloadedNonFinalStateException if the state is not final
    * @return the final number or an error
    */ 
  @static
  private def unload(state: CSKState) : Number | Control.Err =
    state.control match {
      case Control.Value(Expression.Num(n)) => n
      case Control.Value(_) => throw new UnloadedNonFinalStateException("Machine still evaluating expression")
      case Control.Err(e) => Control.Err(e)
      case Control.Search => throw new UnloadedNonFinalStateException("Machine still searching")
    }
