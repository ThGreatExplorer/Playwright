package ast

object ConverterToWE:

    def programToWE(p: CleanProgram): ProgramWE = WE.Node( p match
        case Program[Clean](clss, decls, stmts, expr) => 
            Program[WE](
                clss.map(classToWE),
                decls.map(declToWE),
                stmts.map(stmtToWE),
                exprToWE(expr)
            )
        )
    
    def classToWE(c: CleanClass): ClassWE = WE.Node(c match
        case Class[Clean](cname, fields, methods) => 
            Class[WE](
                nameToWE(cname),
                fields.map(nameToWE),
                methods.map(methodToWE)
            )
        )
    
    def methodToWE(m: CleanMethod): MethodWE = WE.Node( m match
        case Method[Clean](mname, params, decls, stmts, expr) => 
            Method[WE](
                nameToWE(mname),
                params.map(nameToWE),
                decls.map(declToWE),
                stmts.map(stmtToWE),
                exprToWE(expr)
            )
        )
    
    def declToWE(d: CleanDecl): DeclWE = WE.Node( d match
        case Decl[Clean](varDecl, rhs) => 
            Decl[WE](
                nameToWE(varDecl),
                exprToWE(rhs)
            )
        )
    
    def stmtToWE(s: CleanStmt): StmtWE = WE.Node( s match
        case Stmt.Assign[Clean](lhs, rhs) =>
            Stmt.Assign[WE](
                varRefToWE(lhs), 
                exprToWE(rhs))
            
        case Stmt.Ifelse(guard, tbranch, ebranch) =>
            Stmt.Ifelse[WE](
                exprToWE(guard),
                blockToWE(tbranch),
                blockToWE(ebranch)
            )
            
        case Stmt.While(guard, body) =>
            Stmt.While[WE](
                exprToWE(guard), 
                blockToWE(body)
            )
            
        case Stmt.FieldAssign(instance, field, rhs) =>
            Stmt.FieldAssign[WE](
                varRefToWE(instance),
                nameToWE(field),
                exprToWE(rhs)
            )
        )
    
    def blockToWE(b: CleanBlock): BlockWE = WE.Node( b match 
        case Block.One(stmt) =>
            Block.One[WE](stmtToWE(stmt))
            
        case Block.Many(decls, stmts) =>
            Block.Many[WE](
                decls.map(declToWE),
                stmts.map(stmtToWE)
            )
        )
    
    
    def exprToWE(e: CleanExpr): ExprWE = WE.Node(e match 
        case Expr.Num(n) => 
            Expr.Num[WE](n)
            
        case Expr.Var(x) => 
            Expr.Var[WE](varRefToWE(x))
            
        case Expr.BinOpExpr(lhs, op, rhs) =>
            Expr.BinOpExpr[WE](
                varRefToWE(lhs), 
                op, 
                varRefToWE(rhs)
            )
            
        case Expr.NewInstance(cname, args) =>
            Expr.NewInstance[WE](
                nameToWE(cname),
                args.map(varRefToWE)
            )
            
        case Expr.GetField(instance, field) =>
            Expr.GetField[WE](
                varRefToWE(instance), 
                nameToWE(field)
            )
            
        case Expr.CallMethod(instance, method, args) =>
            Expr.CallMethod[WE](
                varRefToWE(instance),
                nameToWE(method),
                args.map(varRefToWE)
            )
            
        case Expr.IsInstanceOf(instance, cname) =>
            Expr.IsInstanceOf[WE](
                varRefToWE(instance), 
                nameToWE(cname)
            )
        )
    
    def varRefToWE(v: CleanVarRef): VarRefWE = 
        WE.Node(v)
    
    def nameToWE(n: CleanName): NameWE = 
        WE.Node(n)
    

