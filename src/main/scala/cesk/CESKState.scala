package cesk

import scala.collection.mutable.{Stack}
import scala.collection.immutable.Map
import ast._
import util.{UnreachableStateException, UnreachablePatternMatch}
import util.EPSILON
import scala.compiletime.ops.double

enum Control:
  case Expr(e : CleanExpr)
  case Value(n : Double)
  case Err(e : RuntimeError)
  case Search

type Loc = Integer

// Think of trait as a type defintion
trait Env:
  def updatedEnv(x : String, loc : Loc) : Env
  def getLoc(x : String) : Loc

// Object is a factory that creates instances of something typed with Env
object Env:

  // Factory methods that return instances of the trait
  def apply(entries: (String, Loc)*): Env = 
    new MapEnv(Map(entries*))

  // Actual class we instantiate that is hidden from users
  private class MapEnv(underlying : Map[String, Loc]) extends Env {
    
    def updatedEnv(x : String, loc : Loc) : Env =
      new MapEnv(underlying.updated(x, loc))

    def getLoc(x : String) : Loc =
      underlying.get(x) match
        case Some(loc) => loc
        case None =>   
          throw new UnreachablePatternMatch("Should never happen: variable " + x + " not found in environment")    
  }


trait Store:
  def insertValAtNewLoc(n: Double) : (Store, Loc)
  def updatedStore(loc: Loc, n: Double) : Store
  def getVal(loc : Loc) : Double

object Store:

  def apply(entries: (Loc, Double)*): Store = 
    new MapStore(Map(entries*))

  private val locactionGenerator = Iterator.from(0)
  private def freshLoc() : Loc = locactionGenerator.next()

  private class MapStore(underlying : Map[Loc, Double]) extends Store {

    def insertValAtNewLoc(n : Double) : (Store, Loc) =
      val loc = freshLoc();
      val newStore = new MapStore(underlying.updated(loc, n))
      (newStore, loc)

    def updatedStore(loc : Loc, n : Double) : Store =
      new MapStore(underlying.updated(loc, n))

    def getVal(loc : Loc) : Double =
      underlying.get(loc) match
        case Some(n) => n
        case None =>   
          throw new UnreachablePatternMatch("Should never happen: location " + loc + " not found in store")    
  }

// A closure combines a program AST with an environment. The AST 
// represents the remaining instructions of either the top-most program 
// (bottom-most stack frame) or a nested block (all others); if the AST 
// represents a nested block, the expression field is filled with a dummy
// value. The environment represents the meaning of variables in the 
// surrounding context.
type Closure = (ProgFrame, Env)

case class ProgFrame(
  decls: List[CleanDecl], 
  stmts: List[CleanStmt | CleanBlock], 
  expr: CleanExpr | Unit
)

trait KontStack:
  def push(clo : Closure) : KontStack
  def pop : KontStack
  def updateTopProgFrame(newProgFrame : ProgFrame) : KontStack
  def isEmpty : Boolean 
  def top : Closure
  def topProgFrame : ProgFrame
  def topEnv : Env 

object KontStack:

  def apply(entries: Closure*) : KontStack = new StackifiedList(List(entries*))

  def constructWithTL(initProg : CleanProgram): KontStack = initProg match
    case CleanProgram(decls, stmts, expr) =>
      val progClosure = (ProgFrame(decls, stmts, expr), Env())
      val initStack = List(progClosure)
      new StackifiedList(initStack)

  private class StackifiedList(underlying : List[Closure]) extends KontStack {
    def push(clo : Closure) : KontStack = new StackifiedList(clo :: underlying)
    def pop : KontStack = new StackifiedList(underlying.tail)
    def updateTopProgFrame(newProgFrame : ProgFrame) : KontStack = 
      val (_, oldEnv) = underlying.head
      val newClosure = (newProgFrame, oldEnv)
      new StackifiedList(newClosure :: underlying.tail)
  
    def isEmpty : Boolean = underlying.isEmpty
    def top : Closure = underlying.head
    def topProgFrame : ProgFrame = { val (pf, _) = underlying.head; pf }
    def topEnv : Env =  { val (_, env) = underlying.head; env }
  }


final case class CESKState(
  control : Control, 
  env : Env,
  store : Store, 
  kont: KontStack
):
  /**
  * Returns true if the given state is a final state. A final state is either an Error state or 
  * a Success state with Value in Control and an empty KontStack stack.
  *
  * @param state the current state of the CESK machine
  * @return true if the state is final, false otherwise
  */
  def isFinal() : Boolean =
    control match { 
      case Control.Err(_) => kont.isEmpty
      case Control.Value(_) => kont.isEmpty
      case _ => false
    }

  def lookupVar(x : String) : Double = 
    val loc = env.getLoc(x) 
    store.getVal(loc) 

object CESKState:

  /**
    * Performs a single transition of the CSK machine based on the current state (control, store, kont).
    *
    * @return the next state of the CSK machine
    * @throws UnreachableStateException if the current state is malformed or an unexpected state is reached
    */
  def transition(state :CESKState) : CESKState =
    (state.control, state.kont.topProgFrame) match 


      // Expressions ---
      case (Control.Expr(CleanExpr.Var(x)), _) =>
        val num = state.lookupVar(x)
        CESKState(
          Control.Value(num),
          state.env,
          state.store,
          state.kont
        )
      
      case (Control.Expr(CleanExpr.BinOpExpr(CleanExpr.Var(lhs), BinOp, CleanExpr.Var(rhs))), _) =>
        val val1 = state.lookupVar(lhs)
        val val2 = state.lookupVar(rhs)
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
        val loc = state.env.getLoc(lhs)
        CESKState(
          Control.Search,
          state.env,
          state.store.updatedStore(loc, num),
          state.kont.updateTopProgFrame(ProgFrame(Nil, stmts, expr))
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
        val (newStore, newLoc) = state.store.insertValAtNewLoc(n)
        CESKState(
          Control.Search,
          state.env.updatedEnv(id, newLoc),
          newStore,
          state.kont.updateTopProgFrame(ProgFrame(rest, stmts, r))
        )

      // Block Statements
      case (Control.Search, ProgFrame(Nil, CleanBlock.One(stmt) :: rest, r)) =>
        CESKState(
          control = Control.Search,
          env = state.env,
          store = state.store,
          kont = state.kont.updateTopProgFrame(ProgFrame(Nil, stmt :: rest, r)) 
        )
      case (Control.Search, ProgFrame(Nil, CleanBlock.Many(decls, stmts) :: rest, r)) =>
        val newClosure = (ProgFrame(decls, stmts, ()), state.env)
        CESKState(
          control = Control.Search,
          env = state.env,
          store = state.store,
          kont = state.kont.updateTopProgFrame(ProgFrame(Nil, rest, r))
                           .push(newClosure)
        )
      case (Control.Search, ProgFrame(Nil, Nil, ())) =>
        val restoredEnv = state.kont.topEnv
        CESKState(
          control = Control.Search,
          env = restoredEnv,
          store = state.store,
          kont = state.kont.pop
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
        val restoredEnv = state.kont.topEnv
        CESKState(
          control = Control.Value(n),
          env = restoredEnv,
          store = state.store,
          kont = state.kont.pop
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
          + "\nEnvironment: " + state.env.toString() 
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
      kont = KontStack()
    )