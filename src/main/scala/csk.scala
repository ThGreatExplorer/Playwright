import ast._
import scala.annotation.static

enum Control {
  case Value(n : Expression)
  case Err(e : RuntimeError)
  case Search
}

class CSKMachine( var control : Control, var store : Map[String, Double], var kont: Program) {

  def load(prog: Program) : CSKMachine = {
    new CSKMachine(
      control = Control.Search,
      store = Map(),
      kont = prog
    )
  }

  def transition(state: CSKMachine) : CSKMachine = {
    (state.control, state.kont) match {
      case (Control.Search, Program.Prog(Nil, Expression.Num(v))) => {
        new CSKMachine(
          control = Control.Value(Expression.Num(v)),
          store = state.store,
          kont = state.kont
        )
      }
      // in final state
      case (Control.Value(n), Program.Prog(Nil, Expression.Num(v))) => {
        new CSKMachine(
          // should this be n or v?
          control = Control.Value(n),
          store = state.store,
          kont = state.kont
        )
      }
      case (Control.Search, Program.Prog(Nil, Expression.Var(x))) => {
        if state.store.contains(x) then
          new CSKMachine(
            control = Control.Value(state.store(x)),
            store = state.store,
            kont = state.kont
          )
        else
          new CSKMachine(
            control = Control.Err(new VarNotFoundError(x)),
            store = state.store,
            kont = state.kont
          )
      }
      case (Control.Search, Program.Prog(stmts=Statement.Assign(rhs, lhs) :: rest, expr=expr)) => {
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
              kont = Program.Prog(rest, expr)
            )
          case Expression.Err(e) =>
            new CSKMachine(
              control = Control.Err(new UnreachableStateError("RHS of assignment is an error expression -> Passed malformed program to CSK machine")),
              store = state.store,
              kont = state.kont
            )
        }
        
      }
      // expresssions
      
      case Program.Err(e) => new CSKMachine(
          control = Control.Err(new UnreachableStateError("Passed a malformed program to the CSK machine")),
          store = state.store,
          kont = state.kont
      )
    }
  }

  def isFinal(state: CSKMachine) : Boolean = {
    state.control match { }
  }


  def unload(state: CSKMachine) : Number | Control.Err = {
    state.control match {
      case Control.Value(n) => n
      case Control.Err(e) => Control.Err(e)
      case Control.Search() => throw new Exception("Machine still searching")
    }
  }
}