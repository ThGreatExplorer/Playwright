package csk

import scala.collection.mutable.Map
import ast._

class CSKState(var control : Control, var store : Map[String, Double], var kont: Kont):

  /**
    * Performs a single transition of the CSK machine based on the current state (control, store, kont).
    *
    * @return the next state of the CSK machine
    * @throws UnreachableStateException if the current state is malformed or an unexpected state is reached
    */
  def transition() : CSKState =
    (this.control, this.kont) match 
      // assignment statements
      case (Control.Search, Kont.Prog(Nil, expr)) => 
        new CSKState(
          control = Control.Value(expr),
          store = this.store,
          kont = this.kont
        )
      case (Control.Search, Kont.Prog(Statement.Assign(lhs, rhs) :: rest, expr)) =>
        new CSKState(
          control = Control.Value(rhs),
          store = this.store,
          kont = this.kont
        )
      case (Control.Value(Expression.Num(n)), Kont.Prog(Statement.Assign(lhs, rhs) :: rest, expr)) => 
        lhs match 
          case Expression.Var(x) => 
            new CSKState(
              control = Control.Search,
              store = this.store + (x -> n),
              kont = Kont.Prog(rest, expr)
            )
          case Expression.Err(e) => throw new UnreachableStateException("Assignment to malformed variable, passed malformed program into CSK machine")
        

      // While Loops
      case (Control.Value(Expression.Num(n)), Kont.Prog(Statement.While(tst, body) :: rest, expr)) =>
        if (n == 0) 
          new CSKState(
            control = Control.Search,
            store = this.store,
            kont = Kont.Prog(body :: Statement.While(tst, body) :: rest, expr)
          )
        else 
          new CSKState(
            control = Control.Search,
            store = this.store,
            kont = Kont.Prog(rest, expr)
          )
      case (Control.Search, Kont.Prog(Statement.While(tst, body) :: rest, expr)) =>
        new CSKState(
          control = Control.Value(tst),
          store = this.store,
          kont = Kont.Prog(Statement.While(tst, body) :: rest, expr)
        )

      // Conditionals
      case (Control.Value(Expression.Num(n)), Kont.Prog(Statement.Ifelse(tst, thn, els) :: rest, expr)) =>
        if (n == 0) 
          new CSKState(
            control = Control.Search,
            store = this.store,
            kont = Kont.Prog(thn :: rest, expr)
          )
        else 
          new CSKState(
            control = Control.Search,
            store = this.store,
            kont = Kont.Prog(els :: rest, expr)
          )
      case (Control.Search, Kont.Prog(Statement.Ifelse(tst, thn, els) :: rest, expr)) =>
        new CSKState(
          control = Control.Value(tst),
          store = this.store,
          kont = Kont.Prog(Statement.Ifelse(tst, thn, els) :: rest, expr)
        )

      // Block Statements
      case (Control.Search, Kont.Prog(Block.One(stmt) :: rest, expr)) =>
        new CSKState(
          control = Control.Search,
          store = this.store,
          kont = Kont.Prog(stmt :: rest, expr)
        )
      case (Control.Search, Kont.Prog(Block.Many(stmts) :: rest, expr)) =>
        new CSKState(
          control = Control.Search,
          store = this.store,
          kont = Kont.Prog(stmts ::: rest, expr)
        )

      // Expresssions
      case (Control.Value(Expression.Var(x)), _) =>
        if this.store.contains(x) then
          new CSKState(
            control = Control.Value(Expression.Num(this.store(x))),
            store = this.store,
            kont = this.kont
          )
        else
          constructErrorState(new RuntimeError.VarNotFound("Variable " + x + " not found in store"))
      case (Control.Value(Expression.Add(lhs, rhs)), _) => 
        (lhs, rhs) match
          case (Expression.Var(x), Expression.Var(y)) => 
            if this.store.contains(x) && this.store.contains(y) then
              new CSKState(
                control = Control.Value(Expression.Num(this.store(x) + this.store(y))),
                store = this.store,
                kont = this.kont
              )
            else
              constructErrorState(new RuntimeError.VarNotFound("Variable " + (if !this.store.contains(x) then x else y) + " not found in store"))

          case (_, _) =>
            throw new UnreachableStateException("Add expression contains non-variable operands, passed malformed program into CSK machine")
      case (Control.Value(Expression.Div(lhs, rhs)), _) =>
        (lhs, rhs) match
          case (Expression.Var(x), Expression.Var(y)) =>
            if this.store.contains(x) && this.store.contains(y) then
              if this.store(y) != 0.0 then
                new CSKState(
                  control = Control.Value(Expression.Num(this.store(x) / this.store(y))),
                  store = this.store,
                  kont = this.kont
                )
              else
                constructErrorState(new RuntimeError.DivisionByZero("Division by zero error in expression: " + x + " / " + y))
            else
              constructErrorState(new RuntimeError.VarNotFound("Variable " + (if !this.store.contains(x) then x else y) + " not found in store"))
          case (_, _) =>
            throw new UnreachableStateException("Div expression contains non-variable operands, passed malformed program into CSK machine")
      case (Control.Value(Expression.Equals(lhs, rhs)), _) =>
        (lhs, rhs) match
          case (Expression.Var(x), Expression.Var(y)) =>
            if this.store.contains(x) && this.store.contains(y) then
              new CSKState(
                // intentionally using 0.0 for true and 1.0 for false to match if0 and while0 spec
                control = Control.Value(Expression.Num(if this.store(x) == this.store(y) then 0.0 else 1.0)),
                store = this.store,
                kont = this.kont
              )
            else
              constructErrorState(new RuntimeError.VarNotFound("Variable " + (if !this.store.contains(x) then x else y) + " not found in store"))
          case (_, _) =>
            throw new UnreachableStateException("Equals expression contains non-variable operands, passed malformed program into CSK machine")

      case _ =>
        throw new UnreachableStateException("Unknown state reached in CSK machine transition function " + this.control.toString() + " |" + this.kont.toString() + "| " + this.store.toString())

  /**
    * Helper method to construct an error state
    *
    * @param e the RuntimeError
    * @return a CSKState in the error state
    */
  private def constructErrorState(e: RuntimeError) : CSKState =
    new CSKState(
      control = Control.Err(e),
      store = Map(),
      kont = Kont.Empty
    )