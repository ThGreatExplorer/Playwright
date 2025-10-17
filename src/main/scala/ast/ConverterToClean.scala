package ast

import util.traverse

object ConverterToClean:

    def progToClean(prog: ProgramWE): Option[CleanProgram] = prog match 
        case WE.Err(_) => None

        case WE.Node(Program(clss, decls, stmts, expr)) => 
            for
                clss  <- clss.traverse(classWEToClean)
                decls <- decls.traverse(declWEToClean)
                stmts <- stmts.traverse(stmtWEToClean)
                expr  <- exprWEToClean(expr)
            yield 
                Program[Clean](clss, decls, stmts, expr)
        
    private def classWEToClean(c: ClassWE): Option[CleanClass] = c match 
        case WE.Err(_) => None

        case WE.Node(Class(cname, fields, methods)) =>
            for 
                cname <- nameWEToClean(cname)
                fields <- fields.traverse(nameWEToClean)
                methods <- methods.traverse(methodWEToClean)
            yield 
                Class[Clean](cname, fields, methods)

    private def methodWEToClean(m: MethodWE): Option[CleanMethod] = m match 
        case WE.Err(_) => None

        case WE.Node(Method(mname, params, decls, stmts, expr)) =>
            for 
                mname <- nameWEToClean(mname)
                params <- params.traverse(nameWEToClean)
                decls <- decls.traverse(declWEToClean)
                stmts <- stmts.traverse(stmtWEToClean)
                expr <- exprWEToClean(expr)
            yield
                Method[Clean](mname, params, decls, stmts, expr)
    
    private def declWEToClean(d: DeclWE): Option[CleanDecl] = d match 
        case WE.Err(_) => None

        case WE.Node(Decl(varDecl, rhs)) =>
            for 
                varDecl <- nameWEToClean(varDecl)
                rhs <- exprWEToClean(rhs)
            yield 
                Decl[Clean](varDecl, rhs)

    private def stmtWEToClean(s: StmtWE): Option[CleanStmt] = s match 
        case WE.Err(_) => None

        case WE.Node(Stmt.Assign(lhs, rhs)) => 
            for 
                lhsClean <- varRefWEToClean(lhs)
                rhsClean <- exprWEToClean(rhs)
            yield 
                Stmt.Assign[Clean](lhsClean, rhsClean)

        case WE.Node(Stmt.Ifelse(guard, tbranch, ebranch)) =>
            for
                guardClean <- exprWEToClean(guard)
                tbranchClean <- blockWEToClean(tbranch)
                ebranchClean <- blockWEToClean(ebranch)
            yield 
                Stmt.Ifelse[Clean](guardClean, tbranchClean, ebranchClean)

        case WE.Node(Stmt.While(guard, body)) =>
            for 
                guardClean <- exprWEToClean(guard)
                bodyClean <- blockWEToClean(body)
            yield
                Stmt.While[Clean](guardClean, bodyClean)

        case WE.Node(Stmt.FieldAssign(instance, field, rhs)) =>
            for 
                instanceClean <- varRefWEToClean(instance)
                fieldClean <- nameWEToClean(field)
                rhsClean <- exprWEToClean(rhs)
            yield 
                Stmt.FieldAssign[Clean](instanceClean, fieldClean, rhsClean)
  
    private def blockWEToClean(b: BlockWE): Option[CleanBlock] = b match 
      case WE.Err(_) => None

      case WE.Node(Block.One(stmt)) =>  
        stmtWEToClean(stmt).map(Block.One[Clean])
          
      case WE.Node(Block.Many(decls, stmts)) =>  
        for 
            declsClean <- decls.traverse(declWEToClean)
            stmtsClean <- stmts.traverse(stmtWEToClean)
        yield 
            Block.Many[Clean](declsClean, stmtsClean)

  
    private def exprWEToClean(e: ExprWE): Option[CleanExpr] = e match 
        case WE.Err(_) => None

        case WE.Node(Expr.Num(n)) => 
            Some(Expr.Num[Clean](n))
            
        case WE.Node(Expr.Var(x)) => 
            varRefWEToClean(x).map(Expr.Var[Clean])

        case WE.Node(Expr.BinOpExpr(lhs, op, rhs)) => 
            for 
                lhsClean <- varRefWEToClean(lhs)
                rhsClean <- varRefWEToClean(rhs)
            yield 
                Expr.BinOpExpr[Clean](lhsClean, op, rhsClean)
            
        case WE.Node(Expr.NewInstance(cname, args)) => 
            for 
                cnameClean <- nameWEToClean(cname)
                argsClean <- args.traverse(varRefWEToClean)
            yield
                Expr.NewInstance[Clean](cnameClean, argsClean)
            
        case WE.Node(Expr.GetField(instance, field)) => 
            for
                instanceClean <- varRefWEToClean(instance)
                fieldClean <- nameWEToClean(field)
            yield 
                Expr.GetField[Clean](instanceClean, fieldClean)
            
        case WE.Node(Expr.CallMethod(instance, method, args)) => 
            for 
                instanceClean <- varRefWEToClean(instance)
                methodClean <- nameWEToClean(method)
                argsClean <- args.traverse(varRefWEToClean)
            yield 
                Expr.CallMethod[Clean](instanceClean, methodClean, argsClean)
            
        case WE.Node(Expr.IsInstanceOf(instance, cname)) => 
            for 
                instanceClean <- varRefWEToClean(instance)
                cnameClean <- nameWEToClean(cname)
            yield 
                Expr.IsInstanceOf[Clean](instanceClean, cnameClean)

    private def varRefWEToClean(v: VarRefWE): Option[CleanVarRef] = v match 
        case WE.Err(_) => None
        case WE.Node(vr) => Some(vr)

    private def nameWEToClean(n: NameWE): Option[CleanName] = n match 
        case WE.Err(_) => None
        case WE.Node(name) => Some(name)
