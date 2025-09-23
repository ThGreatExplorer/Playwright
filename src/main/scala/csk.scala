import ast._
import scala.annotation.static
import scala.collection.mutable.Map
import scala.quoted.Expr

enum Control {
  case Value(n : Expression)
  case Err(e : RuntimeError)
  case Search
}

enum Kont {
  case Prog(stmts: List[Statement | Block], expr: Expression)
  case Empty
}

class CSKMachine( var control : Control, var store : Map[String, Double], var kont: Kont) {

  def load(prog: Program) : CSKMachine = {
    new CSKMachine(
      control = Control.Search,
      store = Map(),
      kont = prog match {
        case Program.Prog(stmts, expr) => Kont.Prog(stmts, expr)
        case Program.Err(e) => Kont.Empty
      }
    )
  }

  def transition(state: CSKMachine) : CSKMachine = {
    (state.control, state.kont) match {

      // Assignment Statements
      case (Control.Search, Kont.Prog(Nil, expr)) => {
        new CSKMachine(
          control = Control.Value(expr),
          store = state.store,
          kont = state.kont
        )
      }
      case (Control.Search, Kont.Prog(stmts=Statement.Assign(rhs, lhs) :: rest, expr=expr)) => {
        new CSKMachine(
          control = Control.Value(lhs),
          store = state.store,
          kont = state.kont
        )
      }
      case (Control.Value(Expression.Num(n)), Program.Prog(stmts=Statement.Assign(rhs, lhs) :: rest, expr=expr)) => {
        rhs match {
          case Expression.Var(x) => 
            new CSKMachine(
              control = Control.Search,
              store = state.store + (x -> n),
              kont = Kont.Prog(rest, expr)
            )
          case Expression.Err(e) => _construct_error_state(new RuntimeError.UnreachableState("Assignment to malformed variable, passed malformed program into CSK machine"))
        }
      }

      // While Loops
      case (Control.Value(Expression.Num(n)), Kont.Prog(Statement.While(tst, body) :: rest, expr)) =>
        if (n == 0) 
          new CSKMachine(
            control = Control.Search,
            store = state.store,
            kont = Kont.Prog(body :: Statement.While(tst, body) :: rest, expr)
          )
        else 
          new CSKMachine(
            control = Control.Search,
            store = state.store,
            kont = Kont.Prog(rest, expr)
          )
      case (Control.Search, Kont.Prog(Statement.While(tst, body) :: rest, expr)) =>
        new CSKMachine(
          control = Control.Value(tst),
          store = state.store,
          kont = Kont.Prog(Statement.While(tst, body) :: rest, expr)
        )

      // Conditionals
      case (Control.Value(Expression.Num(n)), Kont.Prog(Statement.Ifelse(tst, thn, els) :: rest, expr)) =>
        if (n == 0) 
          new CSKMachine(
            control = Control.Search,
            store = state.store,
            kont = Kont.Prog(thn :: rest, expr)
          )
        else 
          new CSKMachine(
            control = Control.Search,
            store = state.store,
            kont = Kont.Prog(els :: rest, expr)
          )
      case (Control.Search, Kont.Prog(Statement.Ifelse(tst, thn, els) :: rest, expr)) =>
        new CSKMachine(
          control = Control.Value(tst),
          store = state.store,
          kont = Kont.Prog(Statement.Ifelse(tst, thn, els) :: rest, expr)
        )

      // Block Statements
      case (Control.Search, Kont.Prog(Block.One(stmt) :: rest, expr)) =>
        new CSKMachine(
          control = Control.Search,
          store = state.store,
          kont = Kont.Prog(stmt :: rest, expr)
        )
      case (Control.Search, Kont.Prog(Block.Many(stmts) :: rest, expr)) =>
        new CSKMachine(
          control = Control.Search,
          store = state.store,
          kont = Kont.Prog(stmts ::: rest, expr)
        )

      // Expresssions
      case (Control.Value(Expression.Var(x)), _) => {
        if state.store.contains(x) then
          new CSKMachine(
            control = Control.Value(Expression.Num(state.store(x))),
            store = state.store,
            kont = state.kont
          )
        else
          _construct_error_state(new RuntimeError.VarNotFound("Variable " + x + " not found in store"))
      }
      case (Control.Value(Expression.Add(lhs, rhs)), _) => {
        (lhs, rhs) match {
          case (Expression.Var(x), Expression.Var(y)) => {
            if state.store.contains(x) && state.store.contains(y) then
              new CSKMachine(
                control = Control.Value(Expression.Num(state.store(x) + state.store(y))),
                store = state.store,
                kont = state.kont
              )
            else
              _construct_error_state(new RuntimeError.VarNotFound("Variable " + (if !state.store.contains(x) then x else y) + " not found in store"))
          }
          case (_, _) => {
            _construct_error_state(new RuntimeError.UnreachableState("Add expression contains non-variable operands, passed malformed program into CSK machine"))
          }
        }
      } 
      case (Control.Value(Expression.Div(lhs, rhs)), _) => {
        (lhs, rhs) match {
          case (Expression.Var(x), Expression.Var(y)) => {
            if state.store.contains(x) && state.store.contains(y) then
              if state.store(y) != 0.0 then
                new CSKMachine(
                  control = Control.Value(Expression.Num(state.store(x) / state.store(y))),
                  store = state.store,
                  kont = state.kont
                )
              else
                _construct_error_state(new RuntimeError.DivisionByZero("Division by zero error in expression: " + x + " / " + y))
            else
              _construct_error_state(new RuntimeError.VarNotFound("Variable " + (if !state.store.contains(x) then x else y) + " not found in store"))
          }
          case (_, _) => {
            _construct_error_state(new RuntimeError.UnreachableState("Div expression contains non-variable operands, passed malformed program into CSK machine"))
          }
        }
      }
      case (Control.Value(Expression.Equals(lhs, rhs)), _) => {
        (lhs, rhs) match {
          case (Expression.Var(x), Expression.Var(y)) => {
            if state.store.contains(x) && state.store.contains(y) then
              new CSKMachine(
                // intentionally using 0.0 for true and 1.0 for false to match if0 and while0 spec
                control = Control.Value(Expression.Num(if state.store(x) == state.store(y) then 0.0 else 1.0)),
                store = state.store,
                kont = state.kont
              )
            else
              _construct_error_state(new RuntimeError.VarNotFound("Variable " + (if !state.store.contains(x) then x else y) + " not found in store"))
          }
          case (_, _) => {
            _construct_error_state(new RuntimeError.UnreachableState("Equals expression contains non-variable operands, passed malformed program into CSK machine"))
          }
        }
      }

      case Program.Err(e) => _construct_error_state(new RuntimeError.UnreachableState("Passed a malformed program to the CSK machine"))
    }
  }

  private def _construct_error_state(e: RuntimeError) : CSKMachine = {
    new CSKMachine(
      control = Control.Err(e),
      store = Map(),
      kont = Kont.Empty
    )
  } 

  def isFinal(state: CSKMachine) : Boolean = {
    state.control match { }
  }


  def unload(state: CSKMachine) : Number | Control.Err = {
    state.control match {
      case Control.Value(n) => n
      case Control.Err(e) => Control.Err(e)
      case Control.Search => throw new Exception("Machine still searching")
    }
  }
}