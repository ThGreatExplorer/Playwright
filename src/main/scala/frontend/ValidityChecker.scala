package frontend

import ast._
import util.InputNotExampleException
import util.UnreachablePatternMatch

object ValidityChecker:
    def closedProg(p: CleanProgram): ProgramWE = p match
        case CleanProgram(decls, stmts, expr) => 
            val (validatedDecls, declared) = closedDecls(decls, List(), Set())
            ProgramWE.Prog(
                validatedDecls,
                stmts.map(closedStmt(_, declared)),
                closedExpr(expr, declared)
            )
                    

    def closedDecls(declsRem: List[CleanDecl], declsSoFar: List[DeclWE], dvars: Set[String]) 
        : (List[DeclWE], Set[String]) =  
            declsRem match
                case Nil => (declsSoFar.reverse, dvars)
                case CleanDecl(CleanExpr.Var(x), rhs) :: tail => 
                    val processedDecl = 
                        DeclWE.Def(
                            ExprWE.Var(x),
                            closedExpr(rhs, dvars)
                        )
                    closedDecls(tail, processedDecl :: declsSoFar, dvars.incl(x))

    def closedStmt(stmt: CleanStmt, dvars: Set[String]): StmtWE = stmt match
        case CleanStmt.Assign(id @ CleanExpr.Var(_), rhs) => 
            StmtWE.Assign(
                closedVariable(id, dvars), 
                closedExpr(rhs, dvars)
            )
        case CleanStmt.Ifelse(guard, tbranch, ebranch) =>
            StmtWE.Ifelse(
                closedExpr(guard, dvars), 
                closedBlock(tbranch, dvars), 
                closedBlock(ebranch, dvars) 
            )
        case CleanStmt.While(guard, body) =>
            StmtWE.While(
                closedExpr(guard, dvars),
                closedBlock(body, dvars)
            )
        
    def closedBlock(block: CleanBlock, dvars: Set[String]): BlockWE = block match
        case CleanBlock.One(stmt) => 
            BlockWE.One(closedStmt(stmt, dvars))
        case CleanBlock.Many(decls, stmts) => 
            val (processedDecls, declared) = closedDecls(decls, List(), dvars)
            BlockWE.Many(
                processedDecls, 
                stmts.map(closedStmt(_, declared))
            )
            
    def closedExpr(e: CleanExpr, dvars: Set[String]): ExprWE = e match
        case varExpr @ CleanExpr.Var(b) => 
            closedVariable(varExpr, dvars)

        case CleanExpr.BinOpExpr(lhs @ CleanExpr.Var(_), op, rhs @ CleanExpr.Var(_)) => 
            ExprWE.BinOpExpr(
                closedVariable(lhs, dvars), 
                op,
                closedVariable(rhs, dvars)
            )

        case CleanExpr.Num(n) => ExprWE.Num(n)

    def closedVariable(v: CleanExpr.Var, dvars: Set[String]): VarWE = v match
        case CleanExpr.Var(x) if dvars.contains(x) =>
            ExprWE.Var(x)
        case _ =>
            ExprWE.VarErrNode(VarErr.NotDeclared) 
