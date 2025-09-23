import ast._
import scala.annotation.static

enum Control {
  case Value(n : Double)
  case Err(e : RuntimeError)
  case Search()
}

class CSKMachine( var control : Control, var store : Map[String, Double], var kont: Program) {

  def load(prog: Program) : CSKMachine = {
    new CSKMachine(
      control = Control.Search(),
      store = Map(),
      kont = prog
    )
  }

  def transition(state: CSKMachine) : CSKMachine = {
    state.kont match {
      case Program.Prog(Nil, Expression.Num(v)) => {
        state.control match {
          case Control.Search() => {
            new CSKMachine(
              control = Control.Value(v),
              store = state.store,
              kont = state.kont
            )
          }
          // in final state
          case Control.Value(n) => {
            new CSKMachine(
              control = Control.Value(n),
              store = state.store,
              kont = state.kont
            )
          }
        }
      }
      case Program.Prof(Nil, Expression.Var(x)) => {

      }
      case Program.Err(e) => new CSKMachine(
          control = Control.Err(new UnreachableStateError("Passed a malformed program to the CSK machine")),
          store = state.store,
          kont = state.kont
      )
    }
  }


  def unload(state: CSKMachine) : Control.Value | Control.Err = {
    state.control match {
      case Control.Value(n) => Control.Value(n)
      case Control.Err(e) => Control.Err(e)
      case Control.Search() => throw new Exception("Machine still searching")
    }
  }
}