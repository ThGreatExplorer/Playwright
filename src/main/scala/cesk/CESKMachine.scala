package cesk

import scala.annotation.tailrec

import ast._
import util.{UnreachablePatternMatch, UnreachableStateException}
import util.{=~=, isZero}
import cesk.CESKConst._
import cesk.CESKState.constructErrorState
import main.main

final class CESKMachine(prog: CleanProgram):

  // On construction, we initialize classDefs to keep track of class defintions
  val classDefs = ClassDefs(prog.clss)

  /**
  * Runs a program to completion, returning either the final number or an error
  *
  * @param prog the program AST
  * @return the final number or an error
  * @throws UnreachableStateException if the program is malformed or an unexpected state is reached
  */
  def run : CESKValue | RuntimeError =
    @tailrec
    def loopUntilFinal(state : CESKState) : CESKState = 
      if state.isFinal then 
        state
      else
        val nextState = transition(state)
        loopUntilFinal(nextState)

    val initState = load(prog)
    val finalState = loopUntilFinal(initState)
    unload(finalState)
    
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
  private def unload(state: CESKState) : CESKValue | RuntimeError =
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
    // System.err.println(state.kont.length)
    (state.control, state.kont.top) match 
      // Defintions
      case (Control.Search, ProgFrame(Decl(id, rhs) :: rest, stmts, r)) =>
        CESKState(
          control = Control.Expr(rhs),
          env     = state.env,
          store   = state.store,
          kont    = state.kont
        )
      case (Control.Value(n), ProgFrame(Decl(id, rhs) :: rest, stmts, r)) =>
        val (newStore, newLoc) = state.store.insertValAtNewLoc(n)
        CESKState(
          control = Control.Search,
          env     = state.env.updatedEnv(id, newLoc),
          store   = newStore,
          kont    = state.kont.updateTopProgFrame(ProgFrame(rest, stmts, r))
        )

      // Block Statements
      case (Control.Search, ProgFrame(Nil, StmtBlock.One(stmt) :: rest, r)) =>
        CESKState(
          control  = Control.Search,
          env      = state.env,
          store    = state.store,
          kont     = state.kont.updateTopProgFrame(ProgFrame(Nil, stmt :: rest, r)) 
        )
      case (Control.Search, ProgFrame(Nil, StmtBlock.Many(decls, stmts) :: rest, r)) =>
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

      // Return Expression (PITCH)
      case (Control.Search, ProgFrame(Nil, Nil, expr : CleanExpr)) =>
        val allVars = ExprRenamer.getAllVars(expr)
        val renameMap = ExprRenamer.unqiuifyVarNames(allVars)
        val valLookupMap = 
          allVars.map(varRef => (renameMap(varRef), state.lookupVar(varRef))).toMap

        val outerEnv = state.kont.topEnv
        val (extRestoredEnv, updStore) = valLookupMap.foldLeft((outerEnv, state.store)) { 
          case ((envAcc, storeAcc), (renamedVarRef, correspondingVal)) =>
            val (updStore, newLoc) = storeAcc.insertValAtNewLoc(correspondingVal)
            val updEnv = envAcc.updatedEnv(renamedVarRef, newLoc)
            (updEnv, updStore)
        }

        val renamedExpr = ExprRenamer.renameVars(expr, renameMap)
        CESKState(
          control = Control.Expr(renamedExpr),
          env     = extRestoredEnv,
          store   = updStore,
          kont    = state.kont.pop
        )

      // Assignment Statements
      case (Control.Search, ProgFrame(Nil, Stmt.Assign(lhs, rhs) :: stmts, expr)) =>
        CESKState(
          control = Control.Expr(rhs),
          env     = state.env,
          store   = state.store,
          kont    = state.kont
        )
      case (Control.Value(v), ProgFrame(Nil, Stmt.Assign(lhs, rhs) :: stmts, expr)) =>
        val loc = state.env.getLoc(lhs)
        CESKState(
          control = Control.Search,
          env     = state.env,
          store   = state.store.updatedStore(loc, v),
          kont    = state.kont.updateTopProgFrame(ProgFrame(Nil, stmts, expr))
        )

      // While Loops
      case (Control.Search, ProgFrame(Nil, Stmt.While(grd, body) :: stmts, expr)) =>
        CESKState(
          control = Control.Expr(grd),
          env     = state.env,
          store   = state.store,
          kont    = state.kont
        )
      case (Control.Value(grdVal), ProgFrame(Nil, Stmt.While(grd, body) :: stmts, expr)) =>
        grdVal match
          case num : NumVal if num.isZero() =>
            val loop = Stmt.While[Clean](grd, body)
            CESKState(
              control = Control.Search,
              env     = state.env,
              store   = state.store,
              kont    = state.kont.updateTopProgFrame(ProgFrame(Nil, body :: loop :: stmts, expr))
            )
          case _ =>
            CESKState(
                control = Control.Search,
                env     = state.env,
                store   = state.store,
                kont    = state.kont.updateTopProgFrame(ProgFrame(Nil, stmts, expr))
              )

      // Conditionals
      case (Control.Search, ProgFrame(Nil, Stmt.Ifelse(grd, tbranch, ebranch) :: stmts, expr)) =>
        CESKState(
          control = Control.Expr(grd),
          env     = state.env,
          store   = state.store,
          kont    = state.kont
        )
      case (Control.Value(grdVal), ProgFrame(Nil, Stmt.Ifelse(grd, tbranch, ebranch) :: stmts, expr)) =>
        grdVal match
          case num : NumVal if num.isZero() =>
            CESKState(
              control = Control.Search,
              env     = state.env,
              store   = state.store,
              kont    = state.kont.updateTopProgFrame(ProgFrame(Nil, tbranch :: stmts, expr))
            )
          case _ => 
              CESKState(
                control = Control.Search,
                env     = state.env,
                store   = state.store,
                kont    = state.kont.updateTopProgFrame(ProgFrame(Nil, ebranch :: stmts, expr))
              )
      
      // Field Assignment statement
      case (Control.Search, ProgFrame(Nil, Stmt.FieldAssign(instance, field, rhs) :: stmts, expr)) =>
        CESKState(
          control = Control.Expr(rhs),
          env     = state.env,
          store   = state.store,
          kont    = state.kont  
        )
      case (Control.Value(exprVal), ProgFrame(Nil, Stmt.FieldAssign(instance, field, rhs) :: stmts, expr)) =>
        state.lookupVar(instance) match 
          case obj: ObjectVal =>
            obj.lookupField(field) match
              case Left(err) => 
                constructErrorState(err)
              case Right(_) =>
                obj.updateField(field, exprVal)
                CESKState(
                  control = Control.Search,
                  env     = state.env,
                  store   = state.store,
                  kont    = state.kont.updateTopProgFrame(ProgFrame(Nil, stmts, expr))
                )
          case prx : ProxyVal => 
            prx.conformToFieldType(field, exprVal, classDefs) match
              case Left(err) =>
                constructErrorState(err)
              case Right(conformedVal) => 
                prx.updateField(field, conformedVal)
                CESKState(
                  control = Control.Search,
                  env     = state.env,
                  store   = state.store,
                  kont    = state.kont.updateTopProgFrame(ProgFrame(Nil, stmts, expr))
                )
          case _: NumVal => 
            constructErrorState(RuntimeError.ValNotAnObject)

      // Core Expressions 
      case (Control.Expr(Expr.Num(num)), _) =>
        CESKState(
          control = Control.Value(num),
          env     = state.env,
          store   = state.store,
          kont    = state.kont
        )
      case (Control.Expr(Expr.Var(x)), _) =>
        val num = state.lookupVar(x)
        CESKState(
          control = Control.Value(num),
          env     = state.env,
          store   = state.store,
          kont    = state.kont
        )
      case (Control.Expr(Expr.BinOpExpr(lhs, op, rhs)), _) =>
        (state.lookupVar(lhs), op, state.lookupVar(rhs)) match 
          // Addition
          case (val1 : NumVal, BinOp.Add, val2 : NumVal) => 
            CESKState(
              control = Control.Value(val1 + val2),
              env     = state.env,
              store   = state.store,
              kont    = state.kont
            )
          case (_, BinOp.Add, _) =>
            CESKState.constructErrorState(
              RuntimeError.InvalidVarType("Binop attempted on a non-numeric value.")
            )

          // Division
          case (val1 : NumVal, BinOp.Div, val2 : NumVal) => 
            if val2.isZero() then
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
          case (_, BinOp.Div, _) =>
            CESKState.constructErrorState(
              RuntimeError.InvalidVarType("Binop attempted on a non-numeric value.")
            )

          // Equals
          case (val1 : NumVal, BinOp.Equals, val2 : NumVal) =>
            val result = if val1 =~= val2 then TRUTHY else FALSY
            CESKState(
              control = Control.Value(result),
              env     = state.env,
              store   = state.store,
              kont    = state.kont
            )
          case (val1, BinOp.Equals, val2) =>
            val result = if val1.equals(val2) then TRUTHY else FALSY
            CESKState(
              control = Control.Value(result),
              env     = state.env,
              store   = state.store,
              kont    = state.kont 
            )

      // Object Creation and Inspection
      case (Control.Expr(Expr.NewInstance(cname, args)), _) =>
        val fieldVals = args.map(state.lookupVar(_))
        classDefs.getInstanceOfClass(cname, fieldVals) match
          case Left(err) => 
            constructErrorState(err)
          case Right(objVal) =>
            CESKState(
              control = Control.Value(objVal),
              env     = state.env,
              store   = state.store,
              kont    = state.kont
            ) 
      case (Control.Expr(Expr.IsInstanceOf(x, cname)), _) =>
        state.lookupVar(x) match
          case ObjectVal(lookupCname, _) => 
            val result = if lookupCname == cname then TRUTHY else FALSY
            CESKState(
              control = Control.Value(result),
              env     = state.env,
              store   = state.store,
              kont    = state.kont 
            )
          case ProxyVal(ObjectVal(lookupCname, _), _) => 
            val result = if lookupCname == cname then TRUTHY else FALSY
            CESKState(
              control = Control.Value(result),
              env     = state.env,
              store   = state.store,
              kont    = state.kont 
            )
          case _: NumVal => 
            CESKState(
              control = Control.Value (FALSY),
              env     = state.env,
              store   = state.store,
              kont    = state.kont 
            )
          
      // Field Retrieval
      case (Control.Expr(Expr.GetField(x, field)), _) =>
        state.lookupVar(x) match
          case _: NumVal => 
            constructErrorState(RuntimeError.ValNotAnObject)
          case obj: ObjectVal =>
            obj.lookupField(field) match
              case Left(err) => 
                constructErrorState(err)
              case Right(fieldVal) =>
                CESKState(
                  control = Control.Value(fieldVal),
                  env     = state.env,
                  store   = state.store,
                  kont    = state.kont  
                )
          case prx : ProxyVal => 
            prx.lookupConformingField(field, classDefs) match
              case Left(err) =>
                constructErrorState(err)
              case Right(conformedVal) => 
                CESKState(
                  control = Control.Value(conformedVal),
                  env     = state.env,
                  store   = state.store,
                  kont    = state.kont
                )

      // Method call
      case (Control.Expr(Expr.CallMethod(instance, mname, args)), _) =>
        state.lookupVar(instance) match
          case _: NumVal => 
            constructErrorState(RuntimeError.ValNotAnObject)

          case obj: ObjectVal =>
            val paramVals = args.map(state.lookupVar(_))

            obj.checkMethod(classDefs, mname, paramVals) match
              case Left(err) => 
                constructErrorState(err)
              case Right(MethodDef(paramNames, methodFrame)) => 
                val (newEnv, newStore) = createMethodEnvAndStore(
                  paramNames, paramVals, obj, Env(), state.store
                )

                val methodClosure = (methodFrame, state.env)
                CESKState(
                  control = Control.Search,
                  store   = newStore,
                  env     = newEnv,
                  kont    = state.kont.push(methodClosure)
                )    

          case proxy @ ProxyVal(obj, typ) => 
            val paramVals = args.map(state.lookupVar(_))

            proxy.checkConformingMethod(mname, paramVals, classDefs) match
              case Left(err) => 
                constructErrorState(err)
              case Right((MethodDef(paramNames, methodFrame), conformingParamVals, rangeT)) =>
                val (newEnv, newStore) = createMethodEnvAndStore(
                  paramNames, conformingParamVals, obj, Env(), state.store
                )

                val methodClosure = (methodFrame, state.env)
                CESKState(
                  control = Control.Search,
                  store   = newStore,
                  env     = newEnv,
                  kont    = state.kont.push(rangeT).push(methodClosure)
                )

      // Returning from a proxied method call
      case (Control.Value(v), rType: RangeT) =>
        ProxyVal.conformToType(v, rType, classDefs) match
          case Left(err) => 
            constructErrorState(err)
          case Right(conformedVal) => 
            CESKState(
              Control.Value(conformedVal),
              state.env,
              state.store,
              state.kont.pop
            )

      case _ =>
        throw new UnreachableStateException(
          "Unknown state reached in CESK machine transition function:" + state
        )

  private def createMethodEnvAndStore(
    paramNames: List[String], 
    paramVals: List[CESKValue],
    obj: CESKValue,
    baseEnv: Env,
    baseStore: Store
  ): (Env, Store) =
    val paramLookupMap = paramNames.zip(paramVals).toMap
    val valLookupMap = Map("this" -> obj) ++ paramLookupMap

    valLookupMap.foldLeft((baseEnv, baseStore)) { 
      case ((envAcc, storeAcc), (paramName, paramVal)) =>
        val (updStore, newLoc) = storeAcc.insertValAtNewLoc(paramVal)
        val updEnv = envAcc.updatedEnv(paramName, newLoc)
        (updEnv, updStore)
  }