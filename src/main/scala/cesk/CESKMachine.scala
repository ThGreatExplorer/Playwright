package cesk

import scala.collection.mutable.Map

import ast._
import util.{UnreachablePatternMatch, UnreachableStateException}
import util.InexactComparator._
import cesk.CESKConst._

object CESKMachine:

  /**
  * Runs a program to completion, returning either the final number or an error
  *
  * @param prog the program AST
  * @return the final number or an error
  * @throws UnreachableStateException if the program is malformed or an unexpected state is reached
  */
  def run(prog: CleanProgram) : NumVal | RuntimeError =
    var state = load(prog)
    while !state.isFinal do
      state = transition(state)
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
  private def unload(state: CESKState) : NumVal | RuntimeError =
    state.control match {
      case Control.Value(n) => n
      case Control.Err(e)   => e
      case _ => 
        throw new UnreachablePatternMatch("Should never happen: at unload() Control is not final")
    }

  /**
    * Performs a single transition of the CSK machine based on the current state (control, store, kont).
    *
    * @return the next state of the CSK machine
    * @throws UnreachableStateException if the current state is malformed or an unexpected state is reached
    */
  private def transition(state :CESKState) : CESKState =
    (state.control, state.kont.topProgFrame) match 

      // Defintions
      case (Control.Search, ProgFrame(CleanDecl(id, rhs) :: rest, stmts, r)) =>
        CESKState(
          control = Control.Expr(rhs),
          env     = state.env,
          store   = state.store,
          kont    = state.kont
        )
      case (Control.Value(n), ProgFrame(CleanDecl(CleanVar(id), rhs) :: rest, stmts, r)) =>
        val (newStore, newLoc) = state.store.insertValAtNewLoc(n)
        CESKState(
          control = Control.Search,
          env     = state.env.updatedEnv(id, newLoc),
          store   = newStore,
          kont    = state.kont.updateTopProgFrame(ProgFrame(rest, stmts, r))
        )

      // Block Statements
      case (Control.Search, ProgFrame(Nil, CleanBlock.One(stmt) :: rest, r)) =>
        CESKState(
          control  = Control.Search,
          env      = state.env,
          store    = state.store,
          kont     = state.kont.updateTopProgFrame(ProgFrame(Nil, stmt :: rest, r)) 
        )
      case (Control.Search, ProgFrame(Nil, CleanBlock.Many(decls, stmts) :: rest, r)) =>
        val newClosure = (ProgFrame(decls, stmts, BLOCKFLAG), state.env)
        CESKState(
          control = Control.Search,
          env     = state.env,
          store   = state.store,
          kont    = state.kont.updateTopProgFrame(ProgFrame(Nil, rest, r))
                              .push(newClosure)
        )
      case (Control.Search, ProgFrame(Nil, Nil, BLOCKFLAG)) =>
        val restoredEnv = state.kont.topEnv
        CESKState(
          control = Control.Search,
          env     = restoredEnv,
          store   = state.store,
          kont    = state.kont.pop
        )

      // End Of Program 
      case (Control.Search, ProgFrame(Nil, Nil, expr : CleanExpr)) =>
        CESKState(
          control = Control.Expr(expr),
          env     = state.env,
          store   = state.store,
          kont    = state.kont
        )
      case (Control.Value(n), ProgFrame(Nil, Nil, expr : CleanExpr)) =>
        val restoredEnv = state.kont.topEnv
        CESKState(
          control = Control.Value(n),
          env     = restoredEnv,
          store   = state.store,
          kont    = state.kont.pop
        )

      // Expressions 
      case (Control.Expr(CleanExpr.Var(x)), _) =>
        val num = state.lookupVar(x)
        CESKState(
          control = Control.Value(num),
          env     = state.env,
          store   = state.store,
          kont    = state.kont
        )
      
      case (Control.Expr(CleanExpr.BinOpExpr(CleanExpr.Var(lhs), op @ BinOp, CleanExpr.Var(rhs))), _) =>
        val val1 = state.lookupVar(lhs)
        val val2 = state.lookupVar(rhs)
        op match
          case BinOp.Add =>
            CESKState(
              control = Control.Value(val1 + val2),
              env     = state.env,
              store   = state.store,
              kont    = state.kont
            )    
          case BinOp.Div =>
            if numValIsZero(val2) then
              CESKState.constructErrorState(
                RuntimeError.DivisionByZero(f"Dividing by Zero: $val1 / $val2")
              )
            else
              CESKState(
                control = Control.Value(val1 / val2),
                env     = state.env,
                store   = state.store,
                kont    = state.kont
              ) 
          case BinOp.Equals =>
            val result = if numValsAreEqual(val1, val2) then TRUTHY else FALSY
            CESKState(
              control = Control.Value(result),
              env     = state.env,
              store   = state.store,
              kont    = state.kont
            )

      // Assignment Statements
      case (Control.Search, ProgFrame(Nil, CleanStmt.Assign(CleanExpr.Var(lhs), rhs) :: stmts, expr)) =>
        CESKState(
          control = Control.Expr(rhs),
          env     = state.env,
          store   = state.store,
          kont    = state.kont
        )
      case (Control.Value(num), ProgFrame(Nil, CleanStmt.Assign(CleanExpr.Var(lhs), rhs) :: stmts, expr)) =>
        val loc = state.env.getLoc(lhs)
        CESKState(
          control = Control.Search,
          env     = state.env,
          store   = state.store.updatedStore(loc, num),
          kont    = state.kont.updateTopProgFrame(ProgFrame(Nil, stmts, expr))
        )


      // conditional
      case (Control.Search, ProgFrame(Nil, CleanStmt.Ifelse(guard, tbranch, ebranch) :: stmts, expr)) =>
        CESKState(
          control = Control.Expr(guard),
          env     = state.env,
          store   = state.store,
          kont    = state.kont
        )
      case (Control.Value(tst), ProgFrame(Nil, CleanStmt.Ifelse(guard, tbranch, ebranch) :: stmts, expr)) =>
        if numValIsZero(tst) then
          CESKState(
            control = Control.Search,
            env     = state.env,
            store   = state.store,
            kont    = state.kont.updateTopProgFrame(ProgFrame(Nil, tbranch :: stmts, expr))
          )
        else
          CESKState(
            control = Control.Search,
            env     = state.env,
            store   = state.store,
            kont    = state.kont.updateTopProgFrame(ProgFrame(Nil, ebranch :: stmts, expr))
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