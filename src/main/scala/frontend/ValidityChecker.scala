import ast._
import util.InputNotExampleException
import util.UnreachablePatternMatch

package main.validity

object ValidityChecker:

    def closedProg(p: Program): Program = p match
        case Program.Prog(decls, stmts, expr) => {
            val (validatedDecls, declared) = closedDecls(decls)
            Program.Prog(
                validatedDecls,
                stmts.map(closedStmt(_, declared)),
                closedExpr(expr, declared)
            )
        }            
        case Program.Err(_) => 
            throw new UnreachablePatternMatch("Program Err node at Scope Validation")

    // def closedDecls(decls: List[Declaration]) : (List[Declaration], Set[Expression.Var]) =


    def closedDecl(d: Declaration): Boolean = d match
        case Declaration.Def(lhs, rhs) => hasError(lhs) || hasError(rhs)
        case Declaration.Err(_) => true

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
    
    def hasError(e: Expression): Boolean = e match
        case Expression.Num(_) | Expression.Var(_) => false
        case Expression.Add(lhs, rhs) => hasError(lhs) || hasError(rhs)
        case Expression.Div(lhs, rhs) => hasError(lhs) || hasError(rhs)
        case Expression.Equals(lhs, rhs) => hasError(lhs) || hasError(rhs)
        case Expression.Err(_) => true
