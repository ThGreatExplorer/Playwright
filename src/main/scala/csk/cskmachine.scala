package csk

import ast._
import scala.collection.mutable.Map

object CSKMachine:

  /**
  * Runs a program to completion, returning either the final number or an error
  *
  * @param prog the program AST
  * @return the final number or an error
  * @throws UnreachableStateException if the program is malformed or an unexpected state is reached
  */
  def run(prog: Program) : Number | Control.Err =
    var state = CSKMachine.load(prog)
    while !state.isFinal() do
      state = CSKState.transition(state)
    CSKMachine.unload(state)

  /**
    * Loads a program into the initial state of the CSK machine where the control is set to Search, the store is empty, and the continuation contains all statements and the final expression in order.
    *
    * @param prog the programAST
    * @return the initial CSKState
    * @throws UnreachableStateException if the program is malformed (i.e. there is a parser error)
    */
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
    * Unloads the final result of a computation from a final CSKState.
    * 
    * @param state a final CSKState
    * @throws UnloadedNonFinalStateException if the state is not final
    * @return the final number or an error
    */ 
  private def unload(state: CSKState) : Number | Control.Err =
    state.control match {
      case Control.Value(Expression.Num(n)) => n
      case Control.Value(_) => throw new UnloadedNonFinalStateException("Machine still evaluating expression")
      case Control.Err(e) => Control.Err(e)
      case Control.Search => throw new UnloadedNonFinalStateException("Machine still searching")
    }
