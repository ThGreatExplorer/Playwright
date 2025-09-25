package csk

import scala.collection.mutable.Map
import error.UnreachableStateException
import ast._

enum Control:
  case Expr(s : Expression)
  case Value(n : Double)
  case Err(e : RuntimeError)
  case Search

enum Kont:
  case Prog(stmts: List[Statement | Block], expr: Expression)
  case Empty

case class CSKState(
  control : Control, 
  store : Map[String, Double], 
  kont: Kont
):
  /**
  * Returns true if the given state is a final state where the final state is either an Error state or 
  * a state with a final number and a continuation with only expression left.
  *
  * @param state the current state of the CSK machine
  * @return true if the state is final, false otherwise
  */
  def isFinal() : Boolean =
    (control, kont) match { 
      case (Control.Err(_), Kont.Empty) => true
      case (Control.Value(_), Kont.Prog(Nil, expr)) => true
      case _ => false
    }

object CSKState:

  /**
    * Performs a single transition of the CSK machine based on the current state (control, store, kont).
    *
    * @return the next state of the CSK machine
    * @throws UnreachableStateException if the current state is malformed or an unexpected state is reached
    */
  def transition(state :CSKState) : CSKState =
    (state.control, state.kont) match 

      // Assignment Statements
      case (Control.Search, Kont.Prog(Nil, expr)) => 
        CSKState(
          control = Control.Expr(expr),
          store = state.store,
          kont = state.kont
        )
      case (Control.Search, Kont.Prog(Statement.Assign(lhs, rhs) :: rest, expr)) =>
        CSKState(
          control = Control.Expr(rhs),
          store = state.store,
          kont = state.kont
        )
      case (Control.Value(n), Kont.Prog(Statement.Assign(Expression.Var(x), rhs) :: rest, expr)) => 
        CSKState(
          control = Control.Search,
          store = state.store + (x -> n),
          kont = Kont.Prog(rest, expr)
        )
        

      // While Loops
      case (Control.Value(n), Kont.Prog(Statement.While(tst, body) :: rest, expr)) =>
        if (n == 0) 
          CSKState(
            control = Control.Search,
            store = state.store,
            kont = Kont.Prog(body :: Statement.While(tst, body) :: rest, expr)
          )
        else 
          CSKState(
            control = Control.Search,
            store = state.store,
            kont = Kont.Prog(rest, expr)
          )
      case (Control.Search, Kont.Prog(Statement.While(tst, body) :: rest, expr)) =>
        CSKState(
          control = Control.Expr(tst),
          store = state.store,
          kont = state.kont
        )

      // Conditionals
      case (Control.Value(n), Kont.Prog(Statement.Ifelse(tst, thn, els) :: rest, expr)) =>
        if (n == 0) 
          CSKState(
            control = Control.Search,
            store = state.store,
            kont = Kont.Prog(thn :: rest, expr)
          )
        else 
          CSKState(
            control = Control.Search,
            store = state.store,
            kont = Kont.Prog(els :: rest, expr)
          )
      case (Control.Search, Kont.Prog(Statement.Ifelse(tst, thn, els) :: rest, expr)) =>
        CSKState(
          control = Control.Expr(tst),
          store = state.store,
          kont = state.kont
        )

      // Block Statements
      case (Control.Search, Kont.Prog(Block.One(stmt) :: rest, expr)) =>
        CSKState(
          control = Control.Search,
          store = state.store,
          kont = Kont.Prog(stmt :: rest, expr)
        )
      case (Control.Search, Kont.Prog(Block.Many(stmts) :: rest, expr)) =>
        CSKState(
          control = Control.Search,
          store = state.store,
          kont = Kont.Prog(stmts ::: rest, expr)
        )

      // Expresssions
      case (Control.Expr(Expression.Num(n)), _) => 
        CSKState(
          control = Control.Value(n),
          store = state.store,
          kont = state.kont
        )
      case (Control.Expr(Expression.Var(x)), _) => 
        state.store.get(x) match
          case Some(n) => 
            CSKState(
              control = Control.Value(n),
              store = state.store,
              kont = state.kont
            )
          case None =>
            constructErrorState(
              RuntimeError.VarNotFound("Variable " + x + " not found in store")
            )
      case (Control.Expr(Expression.Add(Expression.Var(x), Expression.Var(y))), _) => 
        (state.store.get(x), state.store.get(y)) match
          case (Some(xNum), Some(yNum)) => 
            CSKState(
              control = Control.Value(xNum + yNum),
              store = state.store,
              kont = state.kont
            )
          case (Some(xNum), None) => 
            constructErrorState(
              RuntimeError.VarNotFound("Variable " + y + " not found in store")
            )
          case _ => 
            constructErrorState(
              RuntimeError.VarNotFound("Variable " + x + " not found in store")
            )
      case (Control.Expr(Expression.Div(Expression.Var(x), Expression.Var(y))), _) =>
        (state.store.get(x), state.store.get(y)) match
          case (Some(xNum), Some(yNum)) => 
            if yNum != 0.0 then
              CSKState(
                control = Control.Value(xNum / yNum),
                store = state.store,
                kont = state.kont
              )
            else
              constructErrorState(
                RuntimeError.DivisionByZero("Division by zero error in expression: " + x + " / " + y)
              )
          case (Some(xNum), None) => 
            constructErrorState(
              RuntimeError.VarNotFound("Variable " + y + " not found in store")
            )
          case _ => 
            constructErrorState(
              RuntimeError.VarNotFound("Variable " + x + " not found in store")
            )
      case (Control.Expr(Expression.Equals(Expression.Var(x), Expression.Var(y))), _) =>
        (state.store.get(x), state.store.get(y)) match
          case (Some(xNum), Some(yNum)) => 
            CSKState(
              // intentionally using 0.0 for true and 1.0 for false to match if0 and while0 spec
              control = Control.Value(if xNum == yNum then 0.0 else 1.0),
              store = state.store,
              kont = state.kont
            )
          case (Some(xNum), None) => 
            constructErrorState(
              RuntimeError.VarNotFound("Variable " + y + " not found in store")
            )
          case _ => 
            constructErrorState(
              RuntimeError.VarNotFound("Variable " + x + " not found in store")
            )

      case _ =>
        throw new UnreachableStateException(
          "Unknown state reached in CSK machine transition function:" 
          + "\nControl: " + state.control.toString() 
          + "\nStore: " + state.store.toString()
          + "\nKont: " + state.kont.toString() 
        )

  /**
    * Helper method to construct an error state
    *
    * @param e the RuntimeError
    * @return a CSKState in the error state
    */
  private def constructErrorState(e: RuntimeError) : CSKState =
    CSKState(
      control = Control.Err(e),
      store = Map(),
      kont = Kont.Empty
    )