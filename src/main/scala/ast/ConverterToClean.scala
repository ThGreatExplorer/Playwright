package ast

import util.traverse

object ConverterToClean:

    // Top Level converters

    def rawSystemToClean(sys : RawSystemWE) : Option[CleanRawSystem] = sys match
        case WE.Err(_) => None
        
        case WE.Node(RawSystem(modules, imports, progb)) => 
            for
                modules  <- modules.traverse(moduleWEToClean)
                imports <- imports.traverse(importToClean)
                progb  <- progBlockWEToClean(progb)
            yield 
                RawSystem[Clean](modules, imports, progb)

    def systemToClean(sys : SystemWE) : Option[CleanSystem] = sys match
        case WE.Err(_) => None
        
        case WE.Node(System(modules, imports, progb, modData)) => 
            for
                modules  <- modules.traverse(moduleWEToClean)
                imports <- imports.traverse(importToClean)
                progb  <- progBlockWEToClean(progb)
            yield 
                System[Clean](modules, imports, progb, modData)

    def progToClean(prog: ProgramWE): Option[CleanProgram] = prog match 
        case WE.Err(_) => None

        case WE.Node(Program(clss, progb)) => 
            for
                clss  <- clss.traverse(classWEToClean)
                progb  <- progBlockWEToClean(progb)
            yield 
                Program[Clean](clss, progb)

    // Module (with types) helpers 

    def moduleWEToClean(m: ModuleWE): Option[CleanModule] = m match 
        case WE.Err(_) => None

        case WE.Node(Module(mname, imports, clas)) =>
            for 
                mname   <- stringWEToClean(mname)
                imports <- imports.traverse(importToClean)
                clas    <- classWEToClean(clas)
            yield 
                Module[Clean](mname, imports, clas)

    def importToClean(i: ImportWE): Option[CleanImport] = i match
        case WE.Err(e) => 
            None
        case WE.Node(Import.Typed(mname, shape)) =>
            for
                mname <- stringWEToClean(mname)
                shape <- shapeWEToClean(shape)
            yield
                Import.Typed[Clean](mname, shape)
        case WE.Node(Import.Untyped(mname)) =>
            untypedImportToClean(WE.Node(Import.Untyped(mname)))

    def untypedImportToClean(i: WE[Import.Untyped[WE]]): Option[Clean[Import.Untyped[Clean]]] = i match
        case WE.Err(e) => 
            None
        case WE.Node(Import.Untyped(mname)) =>
            for 
                mname <- stringWEToClean(mname)
            yield 
                Import.Untyped[Clean](mname)

    def typeWEToClean(typ: TypeWE): Option[CleanType] = typ match
        case WE.Err(_) => None

        case WE.Node(Type.Number()) => Some(Type.Number())

        case WE.Node(Type.Shape(ftypes, mtypes)) => 
            shapeWEToClean(WE.Node(Type.Shape(ftypes, mtypes)))

    def shapeWEToClean(s: ShapeTypeWE): Option[CleanShapeType] = s match 
        case WE.Err(_) => None

        case WE.Node(Type.Shape(ftypes, mtypes)) =>
            for 
                ftypes <- ftypes.traverse(ftypeWEToClean)
                mtypes <- mtypes.traverse(mtypeWEToClean)
            yield 
                Type.Shape[Clean](ftypes, mtypes)

    def ftypeWEToClean(f: FieldTypeWE): Option[CleanFieldType] = f match 
        case WE.Err(_) => None

        case WE.Node(FieldType(fname, ftype)) =>
            for 
                fname <- stringWEToClean(fname)
                ftype <- typeWEToClean(ftype)
            yield 
                FieldType[Clean](fname, ftype)

    def mtypeWEToClean(m: MethodTypeWE): Option[CleanMethodType] = m match 
        case WE.Err(_) => None

        case WE.Node(MethodType(mname, paramTypes, retType)) =>
            for 
                mname      <- stringWEToClean(mname)
                paramTypes <- paramTypes.traverse(typeWEToClean)
                retType    <- typeWEToClean(retType)
            yield 
                MethodType[Clean](mname, paramTypes, retType)

    // Class helpers 
        
    def classWEToClean(c: ClassWE): Option[CleanClass] = c match 
        case WE.Err(_) => None

        case WE.Node(Class(cname, fields, methods, shape)) =>
            for 
                cname <- stringWEToClean(cname)
                fields <- fields.traverse(stringWEToClean)
                methods <- methods.traverse(methodWEToClean)
            yield 
                val cleanShape = shape match
                    case None => None
                    case Some(shapeToClean) => shapeWEToClean(shapeToClean)
                Class[Clean](cname, fields, methods, cleanShape)

    def methodWEToClean(m: MethodWE): Option[CleanMethod] = m match 
        case WE.Err(_) => None

        case WE.Node(Method(mname, params, progb)) =>
            for 
                mname <- stringWEToClean(mname)
                params <- params.traverse(stringWEToClean)
                progb  <- progBlockWEToClean(progb)
            yield
                Method[Clean](mname, params, progb)
    
    // Core helpers

    def progBlockWEToClean(pblock : ProgBlockWE) : Option[CleanProgBlock] = pblock match
        case WE.Err(_) => None
        
        case WE.Node(ProgBlock(decls, stmts, expr)) => 
            for
                decls <- decls.traverse(declWEToClean)
                stmts <- stmts.traverse(stmtWEToClean)
                expr  <- exprWEToClean(expr)
            yield 
                ProgBlock[Clean](decls, stmts, expr)

    def declWEToClean(d: DeclWE): Option[CleanDecl] = d match 
        case WE.Err(_) => None

        case WE.Node(Decl(varDecl, rhs)) =>
            for 
                varDecl <- stringWEToClean(varDecl)
                rhs <- exprWEToClean(rhs)
            yield 
                Decl[Clean](varDecl, rhs)

    def stmtWEToClean(s: StmtWE): Option[CleanStmt] = s match 
        case WE.Err(_) => None

        case WE.Node(Stmt.Assign(lhs, rhs)) => 
            for 
                lhsClean <- stringWEToClean(lhs)
                rhsClean <- exprWEToClean(rhs)
            yield 
                Stmt.Assign[Clean](lhsClean, rhsClean)

        case WE.Node(Stmt.Ifelse(guard, tbranch, ebranch)) =>
            for
                guardClean <- exprWEToClean(guard)
                tbranchClean <- stmtBlockWEToClean(tbranch)
                ebranchClean <- stmtBlockWEToClean(ebranch)
            yield 
                Stmt.Ifelse[Clean](guardClean, tbranchClean, ebranchClean)

        case WE.Node(Stmt.While(guard, body)) =>
            for 
                guardClean <- exprWEToClean(guard)
                bodyClean <- stmtBlockWEToClean(body)
            yield
                Stmt.While[Clean](guardClean, bodyClean)

        case WE.Node(Stmt.FieldAssign(instance, field, rhs)) =>
            for 
                instanceClean <- stringWEToClean(instance)
                fieldClean <- stringWEToClean(field)
                rhsClean <- exprWEToClean(rhs)
            yield 
                Stmt.FieldAssign[Clean](instanceClean, fieldClean, rhsClean)
  
    def stmtBlockWEToClean(b: StmtBlockWE): Option[CleanStmtBlock] = b match 
      case WE.Err(_) => None

      case WE.Node(StmtBlock.One(stmt)) =>  
        stmtWEToClean(stmt).map(StmtBlock.One[Clean])
          
      case WE.Node(StmtBlock.Many(decls, stmts)) =>  
        for 
            declsClean <- decls.traverse(declWEToClean)
            stmtsClean <- stmts.traverse(stmtWEToClean)
        yield 
            StmtBlock.Many[Clean](declsClean, stmtsClean)
  
    def exprWEToClean(e: ExprWE): Option[CleanExpr] = e match 
        case WE.Err(_) => None

        case WE.Node(Expr.Num(n)) => 
            Some(Expr.Num[Clean](n))
            
        case WE.Node(Expr.Var(x)) => 
            stringWEToClean(x).map(Expr.Var[Clean])

        case WE.Node(Expr.BinOpExpr(lhs, op, rhs)) => 
            for 
                lhsClean <- stringWEToClean(lhs)
                rhsClean <- stringWEToClean(rhs)
            yield 
                Expr.BinOpExpr[Clean](lhsClean, op, rhsClean)
            
        case WE.Node(Expr.NewInstance(cname, args)) => 
            for 
                cnameClean <- stringWEToClean(cname)
                argsClean <- args.traverse(stringWEToClean)
            yield
                Expr.NewInstance[Clean](cnameClean, argsClean)
            
        case WE.Node(Expr.GetField(instance, field)) => 
            for
                instanceClean <- stringWEToClean(instance)
                fieldClean <- stringWEToClean(field)
            yield 
                Expr.GetField[Clean](instanceClean, fieldClean)
            
        case WE.Node(Expr.CallMethod(instance, method, args)) => 
            for 
                instanceClean <- stringWEToClean(instance)
                methodClean <- stringWEToClean(method)
                argsClean <- args.traverse(stringWEToClean)
            yield 
                Expr.CallMethod[Clean](instanceClean, methodClean, argsClean)
            
        case WE.Node(Expr.IsInstanceOf(instance, cname)) => 
            for 
                instanceClean <- stringWEToClean(instance)
                cnameClean <- stringWEToClean(cname)
            yield 
                Expr.IsInstanceOf[Clean](instanceClean, cnameClean)

    def stringWEToClean(v: WE[String]): Option[Clean[String]] = v match 
        case WE.Err(_) => None
        case WE.Node(vr) => Some(vr)
