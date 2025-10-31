package ast

object ConverterToWE:

    // Top Level converters

    def systemToWE(s: CleanSystem): SystemWE = WE.Node( s match
        case System[Clean](modules, imports, progb) => 
            System[WE](
                modules.map(moduleToWE),
                imports.map(stringToWE),
                progBlockToWE(progb)
            )
        )

    def programToWE(p: CleanProgram): ProgramWE = WE.Node( p match
        case Program[Clean](clss, progb) => 
            Program[WE](
                clss.map(classToWE),
                progBlockToWE(progb)
            )
        )
    
    // Module helpers

    def moduleToWE(m : CleanModule): ModuleWE = WE.Node(m match
        case Module[Clean](mname, imports, clas) =>
            Module[WE](
                stringToWE(mname), 
                imports.map(stringToWE),
                classToWE(clas)
            )
        )

    // Class helpers 

    def classToWE(c: CleanClass): ClassWE = WE.Node(c match
        case Class[Clean](cname, fields, methods) => 
            Class[WE](
                stringToWE(cname),
                fields.map(stringToWE),
                methods.map(methodToWE)
            )
        )
    
    def methodToWE(m: CleanMethod): MethodWE = WE.Node( m match
        case Method[Clean](mname, params, progb) => 
            Method[WE](
                stringToWE(mname),
                params.map(stringToWE),
                progBlockToWE(progb)
            )
        )

    // Core helpers

    def progBlockToWE(p : CleanProgBlock): ProgBlockWE = WE.Node(p match
        case ProgBlock[Clean](decls, stmts, expr) => 
            ProgBlock[WE](
                decls.map(declToWE),
                stmts.map(stmtToWE),
                exprToWE(expr)
            )
        )
    
    def declToWE(d: CleanDecl): DeclWE = WE.Node( d match
        case Decl[Clean](varDecl, rhs) => 
            Decl[WE](
                stringToWE(varDecl),
                exprToWE(rhs)
            )
        )
    
    def stmtToWE(s: CleanStmt): StmtWE = WE.Node( s match
        case Stmt.Assign[Clean](lhs, rhs) =>
            Stmt.Assign[WE](
                stringToWE(lhs), 
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
                stringToWE(instance),
                stringToWE(field),
                exprToWE(rhs)
            )
        )
    
    def blockToWE(b: CleanStmtBlock): StmtBlockWE = WE.Node( b match 
        case StmtBlock.One(stmt) =>
            StmtBlock.One[WE](stmtToWE(stmt))
            
        case StmtBlock.Many(decls, stmts) =>
            StmtBlock.Many[WE](
                decls.map(declToWE),
                stmts.map(stmtToWE)
            )
        )
    
    
    def exprToWE(e: CleanExpr): ExprWE = WE.Node(e match 
        case Expr.Num(n) => 
            Expr.Num[WE](n)
            
        case Expr.Var(x) => 
            Expr.Var[WE](stringToWE(x))
            
        case Expr.BinOpExpr(lhs, op, rhs) =>
            Expr.BinOpExpr[WE](
                stringToWE(lhs), 
                op, 
                stringToWE(rhs)
            )
            
        case Expr.NewInstance(cname, args) =>
            Expr.NewInstance[WE](
                stringToWE(cname),
                args.map(stringToWE)
            )
            
        case Expr.GetField(instance, field) =>
            Expr.GetField[WE](
                stringToWE(instance), 
                stringToWE(field)
            )
            
        case Expr.CallMethod(instance, method, args) =>
            Expr.CallMethod[WE](
                stringToWE(instance),
                stringToWE(method),
                args.map(stringToWE)
            )
            
        case Expr.IsInstanceOf(instance, cname) =>
            Expr.IsInstanceOf[WE](
                stringToWE(instance), 
                stringToWE(cname)
            )
        )
    
    def stringToWE(v: Clean[String]): WE[String] = 
        WE.Node(v)

