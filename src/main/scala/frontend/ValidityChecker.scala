package frontend

import ast._
import util.InputNotExampleException
import util.UnreachablePatternMatch

object ValidityChecker:
    def closedProg(p: Program): Program = p match
        case Program.Prog(decls, stmts, expr) => {
            val (validatedDecls, declared) = closedDecls(decls, List(), Set())
            Program.Prog(
                validatedDecls,
                stmts.map(closedStmt(_, declared)),
                closedExpr(expr, declared)
            )
        }            
        case Program.Err(_) => 
            throw new UnreachablePatternMatch("Program Err node at Scope Validation")

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
                val (validatedDecls, declared) = closedDecls(decls, List(), dvars)
                (Block.Many(validatedDecls, stmts.map(closedStmt(_, declared))), declared)
            case Block.Err(e) => (Block.Err(e), dvars)

    def closedDecls(declsRem: List[Declaration], declsSoFar: List[Declaration], dvars: Set[Expression.Var]) : (List[Declaration], Set[Expression.Var]) =  
        declsRem match
            case Nil => (declsSoFar.reverse, dvars)
            case Declaration.Def(id @ Expression.Var(_), rhs) :: tail => 
                val processedDecl = 
                    Declaration.Def(
                        id,
                        closedExpr(rhs, dvars)
                    )
                closedDecls(tail, processedDecl :: declsSoFar, dvars.incl(id))
 
            
    def closedExpr(e: Expression, dvars: Set[Expression.Var]): Expression = e match
        case varExpr @ Expression.Var(b) => 
            closedVariable(varExpr, dvars)

        case Expression.BinOpExpr(lhs @ Expression.Var(_), op, rhs @ Expression.Var(_)) => 
            Expression.BinOpExpr(
                closedVariable(lhs, dvars), 
                op,
                closedVariable(rhs, dvars)
            )
        
        case e => e

    def closedVariable(varExpr: Expression.Var, dvars: Set[Expression.Var]): Expression.Var | Expression.Err = 
        if dvars.contains(varExpr) then 
            varExpr
        else 
            Expression.Err(ExprErr.ExprVarNotDeclared) 
