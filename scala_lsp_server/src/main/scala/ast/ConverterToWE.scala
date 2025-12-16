package ast

object ConverterToWE:

    // Top Level converters

    def rawSystemToWE(s: CleanRawSystem): RawSystemWE = WE.Node( s match
        case RawSystem[Clean](modules, imports, progb) => 
            RawSystem[WE](
                modules.map(moduleToWE),
                imports.map(importToWE),
                progBlockToWE(progb)
            )
        )

    def systemToWE(s: CleanSystem): SystemWE = WE.Node( s match
        case System[Clean](modules, imports, progb, modData) => 
            System[WE](
                modules.map(moduleToWE),
                imports.map(importToWE),
                progBlockToWE(progb), 
                modData
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

    def moduleToWE(m : CleanModule): ModuleWE = m match
        case Module(mname, imports, clas) =>
            WE.Node(Module(
                stringToWE(mname), 
                imports.map(importToWE),
                classToWE(clas)
            ))
    
    def importToWE(i : CleanImport): ImportWE = i match
        case Import.Typed(mname, shape) =>
            WE.Node(Import.Typed(
                stringToWE(mname),
                shapeToWE(shape)
            ))
        case Import.Untyped(mname) =>
            untypedImportToWE(Import.Untyped(mname))
    
    def untypedImportToWE(i : Clean[Import.Untyped[Clean]]): WE[Import.Untyped[WE]] = i match
        case Import.Untyped(mname) =>
            WE.Node(Import.Untyped(stringToWE(mname)))

    def typeToWE(typ : CleanType) : TypeWE = typ match
        case Type.Number() =>  WE.Node(Type.Number())
        case s @ Type.Shape[Clean](ftypes, mtypes) => shapeToWE(s)

    def shapeToWE(s : CleanShapeType) : ShapeTypeWE = WE.Node(s match
        case Type.Shape[Clean](ftypes, mtypes) =>
            Type.Shape[WE](
                ftypes.map(ftypeToWE),
                mtypes.map(mtypeToWE),
            )
    )

    def optionalShapeToWE(s : Option[CleanShapeType]) : Option[ShapeTypeWE] = s match
        case Some(value) => Some(shapeToWE(value))
        case None => None

    def ftypeToWE(f : CleanFieldType) : FieldTypeWE = WE.Node(f match
        case FieldType[Clean](fname, ftype) =>
            FieldType[WE](
                stringToWE(fname),
                typeToWE(ftype)
            )
    )

    def mtypeToWE(m : CleanMethodType) : MethodTypeWE = WE.Node(m match
        case MethodType[Clean](mname, paramTypes, retType) =>
            MethodType[WE](
                stringToWE(mname),
                paramTypes.map(typeToWE),
                typeToWE(retType)
            )
    )

    // Class helpers 

    def classToWE(c: CleanClass): ClassWE = WE.Node(c match
        case Class[Clean](cname, fields, methods, shape) => 
            val shapeWE = optionalShapeToWE(shape)
            
            Class[WE](
                stringToWE(cname),
                fields.map(stringToWE),
                methods.map(methodToWE),
                shapeWE
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

