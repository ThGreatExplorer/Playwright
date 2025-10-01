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

    def closedDecls(declsRem: List[Declaration], declsSoFar: List[Declaration], dvars: Set[Expression.Var]) 
        : (List[Declaration], Set[Expression.Var]) =  
            declsRem match
                case Nil => (declsSoFar.reverse, dvars)
                case Declaration.Def(id @ Expression.Var(_), rhs) :: tail => 
                    val processedDecl = 
                        Declaration.Def(
                            id,
                            closedExpr(rhs, dvars)
                        )
                    closedDecls(tail, processedDecl :: declsSoFar, dvars.incl(id))
                case Declaration.Err(_) => 
                    throw new UnreachablePatternMatch("Declaration Err node at Scope Validation")

    def closedStmt(stmt: Statement, dvars: Set[Expression.Var]): Statement = stmt match
        case Statement.Assign(id @ Expression.Var(_), rhs) => 
            Statement.Assign(
                closedVariable(id, dvars), 
                closedExpr(rhs, dvars)
            )
        case Statement.Ifelse(guard, tbranch, ebranch) =>
            Statement.Ifelse(
                closedExpr(guard, dvars), 
                closedBlock(tbranch, dvars), 
                closedBlock(ebranch, dvars) 
            )
        case Statement.While(guard, body) =>
            Statement.While(
                closedExpr(guard, dvars),
                closedBlock(body, dvars)
            )
        case Statement.Err(_) => 
            throw new UnreachablePatternMatch("Statement Err node at Scope Validation")
        
    def closedBlock(block: Block, dvars: Set[Expression.Var]): Block = block match
        case Block.One(stmt) => 
            Block.One(closedStmt(stmt, dvars))
        case Block.Many(decls, stmts) => 
            val (processedDecls, declared) = closedDecls(decls, List(), dvars)
            Block.Many(
                processedDecls, 
                stmts.map(closedStmt(_, declared))
            )
        case Block.Err(_) => 
            throw new UnreachablePatternMatch("Block Err node at Scope Validation")
            
    def closedExpr(e: Expression, dvars: Set[Expression.Var]): Expression = e match
        case varExpr @ Expression.Var(b) => 
            closedVariable(varExpr, dvars)

        case Expression.BinOpExpr(lhs @ Expression.Var(_), op, rhs @ Expression.Var(_)) => 
            Expression.BinOpExpr(
                closedVariable(lhs, dvars), 
                op,
                closedVariable(rhs, dvars)
            )

        case n @ Expression.Num(_) => n

        case Expression.Err(_) => 
            throw new UnreachablePatternMatch("Expression Err node at Scope Validation")

    def closedVariable(varExpr: Expression.Var, dvars: Set[Expression.Var]): Expression.Var | Expression.Err = 
        if dvars.contains(varExpr) then 
            varExpr
        else 
            Expression.Err(ExprErr.ExprVarNotDeclared) 
