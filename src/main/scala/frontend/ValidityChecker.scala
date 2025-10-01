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
                stmts.map(closedStmt(_, declared)),
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

    def hasError(s: Statement): Boolean = s match
        case Statement.Assign(lhs, rhs) => hasError(lhs) || hasError(rhs)
        case Statement.Ifelse(guard, tbranch, ebranch) => 
            hasError(guard) || hasError(tbranch) || hasError(ebranch)
        case Statement.While(guard, body) => hasError(guard) || hasError(body)
        case Statement.Err(_) => true

    def hasError(b: Block): Boolean = b match
        case Block.One(stmt) => hasError(stmt)
        // Similarly, using iterator
        case Block.Many(decls, stmts) => decls.iterator.exists(hasError) || stmts.iterator.exists(hasError)
        case Block.Err(_) => true
    
    def closedExpr(e: Expression, dvars: Set[Expression.Var]): Expression = e match
        case Expression.Num(a) => Expression.Num(a)
        case Expression.Var(b) => if dvars.contains(Expression.Var(b)) then Expression.Var(b) else Expression.Err(ExprErr.ExprVarNotDeclared) 

        case Expression.BinOp(Expression.Var(lhs), Expression.Var(rhs), op) => 
            (dvars.contains(Expression.Var(lhs)), dvars.contains(Expression.Var(rhs))) match
                case (true, true) => Expression.BinOp(Expression.Var(lhs), Expression.Var(rhs), op)
                case (_, _) => Expression.Err(ExprErr.ExprVarNotDeclared) 
        case Expression.Err(e) => Expression.Err(e)
