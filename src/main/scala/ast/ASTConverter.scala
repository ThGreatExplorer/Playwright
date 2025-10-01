package ast

import util.traverse

object ConverterToClean:

    /**
      * Converts an AST that can potentially contain error nodes into one that 
      * is guaranteed to be error-node free. In case this is impossible (i.e.
      * the tree contains Error nodes), we return `None`
      *
      * @param p Program that might have error nodes within its structure
      * @return `Some(CleanProgram)` if the tree is error node free; `None` otherwise
      */
    def progToClean(p: ProgramWE): Option[CleanProgram] = p match 
        case ProgramWE.Err(_) => None
        case ProgramWE.Prog(decls, stmts, expr) =>
            // Scala's syntactic sugar for extracting values from Option (uses flatMap)
            // Good case: 
            // - We evaluate all RHS expressions in the `for` clause. When RHS 
            //   results in `Some(x)`, we bind `x` to LHS name.
            // - If all binds succeed, the `for ... yield ...` block evaluates to 
            //   the expression immediately after `yield`
            // Bad case: 
            // - We evaluate RHS expressions in the `for` clause. If any of the 
            //   expressions on the RHS results in a `None``, the entire block 
            //   `for ... yield ...` short-circuits and returns a `None`
            for
                cleanDecls <- decls.traverse(declToClean)
                cleanStmts <- stmts.traverse(stmtToClean)
                cleanExpr <- exprToClean(expr)
            yield 
                CleanProgram(cleanDecls, cleanStmts, cleanExpr)
    

    def declToClean(d: DeclWE): Option[CleanDecl] = d match 
        case DeclWE.Err(_) => None
        case DeclWE.Def(lhs, rhs) =>
            for 
                cleanLhs <- varToClean(lhs)
                cleanRhs <- exprToClean(rhs)
            yield 
                CleanDecl(cleanLhs, cleanRhs)


    def stmtToClean(s: StmtWE): Option[CleanStmt] = s match 
        case StmtWE.Err(_) => None
        case StmtWE.Assign(lhs, rhs) =>
            for 
                cleanLhs <- varToClean(lhs)
                cleanRhs <- exprToClean(rhs)
            yield 
                CleanStmt.Assign(cleanLhs, cleanRhs)
        case StmtWE.Ifelse(guard, tbranch, ebranch) =>
            for
                cleanGuard <- exprToClean(guard)
                cleanTBranch <- blockToClean(tbranch)
                cleanEBranch <- blockToClean(ebranch)
            yield 
                CleanStmt.Ifelse(cleanGuard, cleanTBranch, cleanEBranch)
        case StmtWE.While(guard, body) =>
            for 
                cleanGuard <- exprToClean(guard)
                cleanBody <- blockToClean(body)
            yield 
                CleanStmt.While(cleanGuard, cleanBody)
    

    def blockToClean(b: BlockWE): Option[CleanBlock] = b match 
        case BlockWE.Err(_) => None
        case BlockWE.One(stmt) =>
            stmtToClean(stmt).map(CleanBlock.One(_))
        case BlockWE.Many(decls, stmts) =>
            for 
                cleanDecls <- decls.traverse(declToClean)
                cleanStmts <- stmts.traverse(stmtToClean)
            yield 
                CleanBlock.Many(cleanDecls, cleanStmts)
        

    def exprToClean(e: ExprWE): Option[CleanExpr] = e match 
        case ExprWE.Num(n) => Some(CleanExpr.Num(n))
        case ExprWE.Var(x) => Some(CleanExpr.Var(x))
        case ExprWE.BinOpExpr(lhs, op, rhs) =>
            for 
                cleanLhs <- varToClean(lhs)
                cleanRhs <- varToClean(rhs)
            yield 
                CleanExpr.BinOpExpr(cleanLhs, op, cleanRhs)
        case _ => None
    

    def varToClean(v: VarWE): Option[CleanVar] = v match 
        case ExprWE.Var(x) => Some(CleanExpr.Var(x))
        case ExprWE.VarErrNode(_) => None
    

