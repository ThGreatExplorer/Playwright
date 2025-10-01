import ast._
import util.InputNotExampleException
import util.UnreachablePatternMatch

package main.validity

object ValidityChecker:

    def closedProg(p: Program): Program = p match
        case Program.Prog(decls, stmts, expr) => {
            val (processedDecls, declared) = closedDecls(decls, Set())
            Program.Prog(
                processedDecls,
                stmts.map(closedStmt(_, declared)),
                closedExpr(expr, declared)
            )
        }            
        case Program.Err(_) => 
            throw new UnreachablePatternMatch("Program Err node at Scope Validation")

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
