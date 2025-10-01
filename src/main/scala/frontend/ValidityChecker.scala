import ast._
import util.InputNotExampleException
import util.UnreachablePatternMatch
import scala.collection.mutable.Set

package main.validity

object ValidityChecker:

    def closedProg(p: Program): Program = p match
        case Program.Prog(decls, stmts, expr) => {
            val (validatedDecls, declared) = closedDecls(decls, Set())
            Program.Prog(
                validatedDecls,
                stmts.map(closedStmts(stmts, declared)),
                closedExpr(expr, declared)
            )
        }            
        case Program.Err(_) => 
            throw new UnreachablePatternMatch("Program Err node at Scope Validation")

    def closedDecls(decls: List[Declaration], dvars: Set[Expression.Var]) : (List[Declaration], Set[Expression.Var]) = decls match
        case Nil => (decls, dvars)
        case h :: t => 
            val (validatedHead, processedVars) = closedDecl(h, dvars)
            val (validatedTail, declaredVars) = closedDecls(t, processedVars)
            (validatedHead :: validatedTail, declaredVars)
    

    def closedDecl(d: Declaration, dvars: Set[Expression.Var]): (Declaration, Set[Expression.Var]) = d match
        case Declaration.Def(Expression.Var(lhs), rhs) => 
            val processedRhs = closedExpr(rhs, dvars)
            dvars.add(Expression.Var(lhs))
            processedRhs match
                case Expression.Err(e) => 
                    (Declaration.Def(Expression.Var(lhs),processedRhs), dvars)
                case _ =>
                    (d, dvars)
        case _ => (d, dvars)

    def closedStmts(stmts: List[Statement], dvars: Set[Expression.Var]): List[Statement] = stmts match
        case Nil => stmts
        case h :: t =>
            (closedStmt(h, dvars) :: closedStmts(t, dvars))

    def closedStmt(stmt: Statement, dvars: Set[Expression.Var]): Statement =
        stmt match
            case Statement.Assign(Expression.Var(lhs), rhs) => 
                val processedLhs: Expression.Err | Expression.Var = if dvars.contains(Expression.Var(lhs)) then Expression.Var(lhs) else Expression.Err(ExprErr.ExprVarNotDeclared)
                Statement.Assign(processedLhs, closedExpr(rhs, dvars))
            case Statement.Ifelse(guard, tbranch, ebranch) =>
                val (processedTBranch, _) = closedBlock(tbranch, dvars)
                val (processedEBranch, _) = closedBlock(ebranch, dvars)     
                Statement.Ifelse(closedExpr(guard, dvars), processedTBranch, processedEBranch)
            case Statement.While(guard, body) =>
                val (processedBody, _) = closedBlock(body, dvars)
                Statement.While(closedExpr(guard, dvars), processedBody)
            case Statement.Err(e) => Statement.Err(e)
        
    def closedBlock(block: Block, dvars: Set[Expression.Var]): (Block, Set[Expression.Var]) = 
        block match
            case Block.One(stmt) => (Block.One(closedStmt(stmt, dvars)), dvars)
            case Block.Many(decls, stmts) => 
                val (validatedDecls, declared) = closedDecls(decls, dvars)
                (Block.Many(validatedDecls, closedStmts(stmts, declared)), declared)
            case Block.Err(e) => (Block.Err(e), dvars)

    def closedExpr(e: Expression, dvars: Set[Expression.Var]): Expression = e match
        case Expression.Num(a) => Expression.Num(a)
        case Expression.Var(b) => if dvars.contains(Expression.Var(b)) then Expression.Var(b) else Expression.Err(ExprErr.ExprVarNotDeclared) 

        case Expression.BinOp(Expression.Var(lhs), Expression.Var(rhs), op) => 
            (dvars.contains(Expression.Var(lhs)), dvars.contains(Expression.Var(rhs))) match
                case (true, true) => Expression.BinOp(Expression.Var(lhs), Expression.Var(rhs), op)
                case (_, _) => Expression.Err(ExprErr.ExprVarNotDeclared) 
        case Expression.Err(e) => Expression.Err(e)
