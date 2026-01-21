package ast

object ConverterToWE:

    // Top Level converters

    def rawSystemToWE(s: CleanRawSystem): RawSystemWE = WE.Node( s match
        case RawSystem[Clean](modules, imports, progb, range) => 
            RawSystem[WE](
                modules.map(moduleToWE),
                imports.map(importToWE),
                progBlockToWE(progb),
                range
            )
        )

    def systemToWE(s: CleanSystem): SystemWE = WE.Node( s match
        case System[Clean](modules, imports, progb, modData, range) => 
            System[WE](
                modules.map(moduleToWE),
                imports.map(importToWE),
                progBlockToWE(progb), 
                modData,
                range
            )
        )

    def programToWE(p: CleanProgram): ProgramWE = WE.Node( p match
        case Program[Clean](clss, progb, range) => 
            Program[WE](
                clss.map(classToWE),
                progBlockToWE(progb),
                range
            )
        )
    
    // Module helpers

    def moduleToWE(m : CleanModule): ModuleWE = m match
        case Module(mname, imports, clas, range) =>
            WE.Node(Module(
                stringToWE(mname), 
                imports.map(importToWE),
                classToWE(clas),
                range
            ))
    
    def importToWE(i : CleanImport): ImportWE = i match
        case Import.Typed(mname, shape, range) =>
            WE.Node(Import.Typed(
                stringToWE(mname),
                shapeToWE(shape),
                range
            ))
        case Import.Untyped(mname, range) =>
            untypedImportToWE(Import.Untyped(mname, range))
    
    def untypedImportToWE(i : Clean[Import.Untyped[Clean]]): WE[Import.Untyped[WE]] = i match
        case Import.Untyped(mname, range) =>
            WE.Node(Import.Untyped(stringToWE(mname), range))

    def typeToWE(typ : CleanType) : TypeWE = typ match
        case Type.Number(range) =>  WE.Node(Type.Number(range))
        case s @ Type.Shape[Clean](ftypes, mtypes, range) => shapeToWE(s)

    def shapeToWE(s : CleanShapeType) : ShapeTypeWE = WE.Node(s match
        case Type.Shape[Clean](ftypes, mtypes, range) =>
            Type.Shape[WE](
                ftypes.map(ftypeToWE),
                mtypes.map(mtypeToWE),
                range
            )
    )

    def optionalShapeToWE(s : Option[CleanShapeType]) : Option[ShapeTypeWE] = s match
        case Some(value) => Some(shapeToWE(value))
        case None => None

    def ftypeToWE(f : CleanFieldType) : FieldTypeWE = WE.Node(f match
        case FieldType[Clean](fname, ftype, range) =>
            FieldType[WE](
                stringToWE(fname),
                typeToWE(ftype),
                range
            )
    )

    def mtypeToWE(m : CleanMethodType) : MethodTypeWE = WE.Node(m match
        case MethodType[Clean](mname, paramTypes, retType, range) =>
            MethodType[WE](
                stringToWE(mname),
                paramTypes.map(typeToWE),
                typeToWE(retType),
                range
            )
    )

    // Class helpers 

    def classToWE(c: CleanClass): ClassWE = WE.Node(c match
        case Class[Clean](cname, fields, methods, shape, range) => 
            val shapeWE = optionalShapeToWE(shape)
            
            Class[WE](
                stringToWE(cname),
                fields.map(stringToWE),
                methods.map(methodToWE),
                shapeWE,
                range
            )
        )
    
    def methodToWE(m: CleanMethod): MethodWE = WE.Node( m match
        case Method[Clean](mname, params, progb, range) => 
            Method[WE](
                stringToWE(mname),
                params.map(stringToWE),
                progBlockToWE(progb),
                range
            )
        )

    // Core helpers

    def progBlockToWE(p : CleanProgBlock): ProgBlockWE = WE.Node(p match
        case ProgBlock[Clean](decls, stmts, expr, range) => 
            ProgBlock[WE](
                decls.map(declToWE),
                stmts.map(stmtToWE),
                exprToWE(expr),
                range
            )
        )
    
    def declToWE(d: CleanDecl): DeclWE = WE.Node( d match
        case Decl[Clean](varDecl, rhs, range) => 
            Decl[WE](
                stringToWE(varDecl),
                exprToWE(rhs),
                range
            )
        )
    
    def stmtToWE(s: CleanStmt): StmtWE = WE.Node( s match
        case Stmt.Assign[Clean](lhs, rhs, range) =>
            Stmt.Assign[WE](
                stringToWE(lhs), 
                exprToWE(rhs),
                range)
            
        case Stmt.Ifelse(guard, tbranch, ebranch, range) =>
            Stmt.Ifelse[WE](
                exprToWE(guard),
                blockToWE(tbranch),
                blockToWE(ebranch),
                range
            )
            
        case Stmt.While(guard, body, range) =>
            Stmt.While[WE](
                exprToWE(guard), 
                blockToWE(body),
                range
            )
            
        case Stmt.FieldAssign(instance, field, rhs, range) =>
            Stmt.FieldAssign[WE](
                stringToWE(instance),
                stringToWE(field),
                exprToWE(rhs),
                range
            )
        )
    
    def blockToWE(b: CleanStmtBlock): StmtBlockWE = WE.Node( b match 
        case StmtBlock.One(stmt, range) =>
            StmtBlock.One[WE](stmtToWE(stmt), range)
            
        case StmtBlock.Many(decls, stmts, range) =>
            StmtBlock.Many[WE](
                decls.map(declToWE),
                stmts.map(stmtToWE),
                range
            )
        )
    
    
    def exprToWE(e: CleanExpr): ExprWE = WE.Node(e match 
        case Expr.Num(n, range) => 
            Expr.Num[WE](n, range)
            
        case Expr.Var(x, range) => 
            Expr.Var[WE](stringToWE(x), range)
            
        case Expr.BinOpExpr(lhs, op, rhs, range) =>
            Expr.BinOpExpr[WE](
                stringToWE(lhs), 
                op, 
                stringToWE(rhs),
                range
            )
            
        case Expr.NewInstance(cname, args, range) =>
            Expr.NewInstance[WE](
                stringToWE(cname),
                args.map(stringToWE),
                range
            )
            
        case Expr.GetField(instance, field, range) =>
            Expr.GetField[WE](
                stringToWE(instance), 
                stringToWE(field),
                range
            )
            
        case Expr.CallMethod(instance, method, args, range) =>
            Expr.CallMethod[WE](
                stringToWE(instance),
                stringToWE(method),
                args.map(stringToWE),
                range
            )
            
        case Expr.IsInstanceOf(instance, cname, range) =>
            Expr.IsInstanceOf[WE](
                stringToWE(instance),
                stringToWE(cname),
                range
            )
        )
    
    def stringToWE(v: Clean[String]): WE[String] = 
        WE.Node(v)
