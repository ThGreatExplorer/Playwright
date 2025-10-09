package cesk

import scala.collection.mutable.Map

import ast._
import util.{UnloadedNonFinalStateException, InputNotExampleException}

object CESKMachine:

  /**
  * Runs a program to completion, returning either the final number or an error
  *
  * @param prog the program AST
  * @return the final number or an error
  * @throws UnreachableStateException if the program is malformed or an unexpected state is reached
  */
  def run(prog: CleanProgram) : Number | RuntimeError =
    var state = load(prog)
    while !state.isFinal() do
      state = CESKState.transition(state)
    unload(state)

  /**
    * Loads a program into the initial state of the CSK machine where the 
    * control is set to Search, the environment and store are empty, and the continuation 
    * contains the entire program prog in the only frame in the stack.
    *
    * @param prog the programAST
    * @return the initial CEKState
    */
  private def load(prog: CleanProgram) : CESKState =
    CESKState(
        control = Control.Search,
        env     = Env(),
        store   = Store(),
        kont    = KontStack.constructWithTL(prog)
      )
     
  /**
    * Unloads the final result of a computation from a final CESKState.
    * 
    * @param state a final CESKState
    * @return the final number or an error
    * @throws UnloadedNonFinalStateException if the state is not final
    */ 
  private def unload(state: CESKState) : Number | RuntimeError =
    state.control match {
      case Control.Value(n) => n
      case Control.Err(e) => e
      case Control.Expr(_) => 
        throw new UnloadedNonFinalStateException("Machine still evaluating expression")
      case Control.Search => 
        throw new UnloadedNonFinalStateException("Machine still searching")
    }
