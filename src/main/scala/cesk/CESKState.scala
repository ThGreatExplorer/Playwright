package cesk

import scala.collection.mutable.{Stack}
import scala.collection.immutable.Map
import ast._
import util.UnreachableStateException
import util.EPSILON
import scala.compiletime.ops.double

enum Control:
  case Expr(e : CleanExpr)
  case Value(n : Double)
  case Err(e : RuntimeError)
  case Search

type Loc = Integer

class Env() {
  private var env : Map[String, Loc] = Map()
}

object Env {

  def updateEnv(env : Env, x : String, loc : Loc): Env = 
    env.env = env.env.updated(x, loc)
    env

  def getEnvVal(env : Env, x : String): Loc = 
    env.env.get(x).get
}

class Store() {
  private var store : Map[Loc, Double] = Map()
}

object Store {
  val locactionGenerator = Iterator.from(0)
  def freshLoc() : Loc = locactionGenerator.next()

  def updateStore(store : Store, num : Double): (Loc, Store) =
    val loc = freshLoc();
    store.store = store.store.updated(loc, num)
    (loc, store)

  def getValFromStore(store : Store, loc: Loc): Double = 
    store.store.get(loc).get
}

// A closure combines a program AST with an environment. The AST 
// represents the remaining instructions of either the top-most program 
// (bottom-most stack frame) or a nested block (all others); if the AST 
// represents a nested block, the expression field is filled with a dummy
// value. The environment represents the meaning of variables in the 
// surrounding context.
type Closure = (ProgFrame, Env)

case class ProgFrame(decls: List[CleanDecl], stmts: List[CleanStmt | CleanBlock], expr: CleanExpr | Unit)

class Kont() {
  private var k : Stack[Closure] = Stack()
}

object Kont {

  def updateTop(kont : Kont, newProgFrame : ProgFrame) = 
    val (_, oldEnv) = kont.k.top
    kont.k.pop()
    kont.k.push((newProgFrame, oldEnv))
    kont

  def top(kont : Kont) = kont.k.top

  def push(kont : Kont, clo : Closure) = {kont.k.push(clo); kont}

  def pop(kont : Kont) = {val closure = kont.k.pop; (closure, kont)}

  def isEmpty(kont : Kont) = kont.k.isEmpty

}

final case class CESKState(
  control : Control, 
  env : Env,
  store : Store, 
  kont: Kont
):
  /**
  * Returns true if the given state is a final state. A final state is either an Error state or 
  * a Success state with Value in Control and an empty Kont stack.
  *
  * @param state the current state of the CESK machine
  * @return true if the state is final, false otherwise
  */
  def isFinal() : Boolean =
    control match { 
      case Control.Err(_) => Kont.isEmpty(kont)
      case Control.Value(_) => Kont.isEmpty(kont)
      case _ => false
    }

object CESKState:

  /**
    * Performs a single transition of the CSK machine based on the current state (control, store, kont).
    *
    * @return the next state of the CSK machine
    * @throws UnreachableStateException if the current state is malformed or an unexpected state is reached
    */
  def transition(state :CESKState) : CESKState =
    val (topFrame, _) = Kont.top(state.kont)
    (state.control, topFrame) match 


      // Expressions ---
      case (Control.Expr(CleanExpr.Var(x)), _) =>
        val loc = Env.getEnvVal(state.env, x) 
        val num = Store.getValFromStore(state.store, loc) 
        CESKState(
          Control.Value(num),
          state.env,
          state.store,
          state.kont
        )
      
      case (Control.Expr(CleanExpr.BinOpExpr(CleanExpr.Var(lhs), BinOp, CleanExpr.Var(rhs))), _) =>
        val val1 = Store.getValFromStore(state.store, Env.getEnvVal(state.env, lhs));
        val val2 = Store.getValFromStore(state.store, Env.getEnvVal(state.env, rhs));
        BinOp match
          case BinOp.Add =>
            CESKState(
              Control.Value(val1 + val2),
              state.env,
              state.store,
              state.kont
            )    
          case BinOp.Div =>
            if val2 != 0.0 then
              CESKState(
                Control.Value(val1 / val2),
                state.env,
                state.store,
                state.kont
              ) 
            else
              constructErrorState(RuntimeError.DivisionByZero(f"Dividing by Zero: $val1 / $val2"))
          case BinOp.Equals =>
            val diff = math.abs(val1 - val2);
            // 1.0 is true, anything else is false
            val result = if diff < EPSILON then 1.0 else 0.0;
            CESKState(
              Control.Value(result),
              state.env,
              state.store,
              state.kont
            )

      // Assignment Statements
      case (Control.Search, ProgFrame(Nil, CleanStmt.Assign(CleanExpr.Var(lhs), rhs) :: stmts, expr)) =>
        CESKState(
          Control.Expr(rhs),
          state.env,
          state.store,
          state.kont
        )
      case (Control.Value(num), ProgFrame(Nil, CleanStmt.Assign(CleanExpr.Var(lhs), rhs) :: stmts, expr)) =>
        val (_, newStore) = Store.updateStore(state.store, num)
        CESKState(
          Control.Search,
          state.env,
          newStore,
          Kont.updateTop(state.kont, ProgFrame(Nil, stmts, expr))
        )

      // Defintions
      case (Control.Search, ProgFrame(CleanDecl(id, rhs) :: rest, stmts, r)) =>
        CESKState(
          control = Control.Expr(rhs),
          env = state.env,
          store = state.store,
          kont = state.kont
        )
      case (Control.Value(n), ProgFrame(CleanDecl(CleanVar(id), rhs) :: rest, stmts, r)) =>
        val (newLoc, newStore) = Store.updateStore(state.store, n)
        val newEnv = Env.updateEnv(state.env, id, newLoc)
        CESKState(
          control = Control.Search,
          env = newEnv,
          store = newStore,
          kont = Kont.updateTop(state.kont, ProgFrame(rest, stmts, r))
        )

      // Block Statements
      case (Control.Search, ProgFrame(Nil, CleanBlock.One(stmt) :: rest, r)) =>
        CESKState(
          control = Control.Search,
          env = state.env,
          store = state.store,
          kont = Kont.updateTop(state.kont, ProgFrame(Nil, stmt :: rest, r))
        )
      case (Control.Search, ProgFrame(Nil, CleanBlock.Many(decls, stmts) :: rest, r)) =>
        val stashedStack = Kont.updateTop(state.kont, ProgFrame(Nil, rest, r))
        val newClo = (ProgFrame(decls, stmts, ()), state.env)
        CESKState(
          control = Control.Search,
          env = state.env,
          store = state.store,
          kont = Kont.push(stashedStack, newClo)
        )
      case (Control.Search, ProgFrame(Nil, Nil, ())) =>
        val ((_, restoredEnv), restOfStack) = Kont.pop(state.kont)
        CESKState(
          control = Control.Search,
          env = restoredEnv,
          store = state.store,
          kont = restOfStack
        )

      // End Of Program 
      case (Control.Search, ProgFrame(Nil, Nil, expr : CleanExpr)) =>
        CESKState(
          control = Control.Expr(expr),
          env = state.env,
          store = state.store,
          kont = state.kont
        )
      case (Control.Value(n), ProgFrame(Nil, Nil, expr : CleanExpr)) =>
        val ((_, restoredEnv), restOfStack) = Kont.pop(state.kont)
        CESKState(
          control = Control.Value(n),
          env = restoredEnv,
          store = state.store,
          kont = restOfStack
        )

      // // Assignment Statements
      // case (Control.Search, ProgFrame(Nil, expr)) => 
      //   CESKState(
      //     control = Control.Expr(expr),
      //     store = state.store,
      //     kont = state.kont
      //   )
      // case (Control.Search, ProgFrame(Statement.Assign(lhs, rhs) :: rest, expr)) =>
      //   CESKState(
      //     control = Control.Expr(rhs),
      //     store = state.store,
      //     kont = state.kont
      //   )
      // case (Control.Value(n), ProgFrame(Statement.Assign(Expression.Var(x), rhs) :: rest, expr)) => 
      //   CESKState(
      //     control = Control.Search,
      //     store = state.store + (x -> n),
      //     kont = ProgFrame(rest, expr)
      //   )
        

      // // While Loops
      // case (Control.Value(n), ProgFrame(Statement.While(tst, body) :: rest, expr)) =>
      //   if (n == 0) 
      //     CESKState(
      //       control = Control.Search,
      //       store = state.store,
      //       kont = ProgFrame(body :: Statement.While(tst, body) :: rest, expr)
      //     )
      //   else 
      //     CESKState(
      //       control = Control.Search,
      //       store = state.store,
      //       kont = ProgFrame(rest, expr)
      //     )
      // case (Control.Search, ProgFrame(Statement.While(tst, body) :: rest, expr)) =>
      //   CESKState(
      //     control = Control.Expr(tst),
      //     store = state.store,
      //     kont = state.kont
      //   )

      // // Conditionals
      // case (Control.Value(n), ProgFrame(Statement.Ifelse(tst, thn, els) :: rest, expr)) =>
      //   if (n == 0) 
      //     CESKState(
      //       control = Control.Search,
      //       store = state.store,
      //       kont = ProgFrame(thn :: rest, expr)
      //     )
      //   else 
      //     CESKState(
      //       control = Control.Search,
      //       store = state.store,
      //       kont = ProgFrame(els :: rest, expr)
      //     )
      // case (Control.Search, ProgFrame(Statement.Ifelse(tst, thn, els) :: rest, expr)) =>
      //   CESKState(
      //     control = Control.Expr(tst),
      //     store = state.store,
      //     kont = state.kont
      //   )

      // // Expresssions
      // case (Control.Expr(Expression.Num(n)), _) => 
      //   CESKState(
      //     control = Control.Value(n),
      //     store = state.store,
      //     kont = state.kont
      //   )
      // case (Control.Expr(Expression.Var(x)), _) => 
      //   state.store.get(x) match
      //     case Some(n) => 
      //       CESKState(
      //         control = Control.Value(n),
      //         store = state.store,
      //         kont = state.kont
      //       )
      //     case None =>
      //       constructErrorState(
      //         RuntimeError.VarNotFound("Variable " + x + " not found in store")
      //       )
      // case (Control.Expr(Expression.Add(Expression.Var(x), Expression.Var(y))), _) => 
      //   (state.store.get(x), state.store.get(y)) match
      //     case (Some(xNum), Some(yNum)) => 
      //       CESKState(
      //         control = Control.Value(xNum + yNum),
      //         store = state.store,
      //         kont = state.kont
      //       )
      //     case (Some(xNum), None) => 
      //       constructErrorState(
      //         RuntimeError.VarNotFound("Variable " + y + " not found in store")
      //       )
      //     case _ => 
      //       constructErrorState(
      //         RuntimeError.VarNotFound("Variable " + x + " not found in store")
      //       )
      // case (Control.Expr(Expression.Div(Expression.Var(x), Expression.Var(y))), _) =>
      //   (state.store.get(x), state.store.get(y)) match
      //     case (Some(xNum), Some(yNum)) => 
      //       if yNum != 0.0 then
      //         CESKState(
      //           control = Control.Value(xNum / yNum),
      //           store = state.store,
      //           kont = state.kont
      //         )
      //       else
      //         constructErrorState(
      //           RuntimeError.DivisionByZero("Division by zero error in expression: " + x + " / " + y)
      //         )
      //     case (Some(xNum), None) => 
      //       constructErrorState(
      //         RuntimeError.VarNotFound("Variable " + y + " not found in store")
      //       )
      //     case _ => 
      //       constructErrorState(
      //         RuntimeError.VarNotFound("Variable " + x + " not found in store")
      //       )
      // case (Control.Expr(Expression.Equals(Expression.Var(x), Expression.Var(y))), _) =>
      //   (state.store.get(x), state.store.get(y)) match
      //     case (Some(xNum), Some(yNum)) => 
      //       CESKState(
      //         // intentionally using 0.0 for true and 1.0 for false to match if0 and while0 spec
      //         control = Control.Value(if xNum == yNum then 0.0 else 1.0),
      //         store = state.store,
      //         kont = state.kont
      //       )
      //     case (Some(xNum), None) => 
      //       constructErrorState(
      //         RuntimeError.VarNotFound("Variable " + y + " not found in store")
      //       )
      //     case _ => 
      //       constructErrorState(
      //         RuntimeError.VarNotFound("Variable " + x + " not found in store")
      //       )

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
    * @return a CESKState in the error state
    */
  private def constructErrorState(e: RuntimeError) : CESKState =
    CESKState(
      control = Control.Err(e),
      env = Env(),
      store = Store(),
      kont = Kont()
    )