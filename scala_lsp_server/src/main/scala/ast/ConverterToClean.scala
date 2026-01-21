package ast

import util.traverse

object ConverterToClean:

    // Top Level converters

    def rawSystemToClean(sys : RawSystemWE) : Option[CleanRawSystem] = sys match
        case WE.Err(_) => None
        
        case WE.Node(RawSystem(modules, imports, progb, range)) => 
            for
                modules  <- modules.traverse(moduleWEToClean)
                imports <- imports.traverse(importToClean)
                progb  <- progBlockWEToClean(progb)
            yield 
                RawSystem[Clean](modules, imports, progb, range)

    def systemToClean(sys : SystemWE) : Option[CleanSystem] = sys match
        case WE.Err(_) => None
        
        case WE.Node(System(modules, imports, progb, modData, range)) => 
            for
                modules  <- modules.traverse(moduleWEToClean)
                imports <- imports.traverse(importToClean)
                progb  <- progBlockWEToClean(progb)
            yield 
                System[Clean](modules, imports, progb, modData, range)

    def progToClean(prog: ProgramWE): Option[CleanProgram] = prog match 
        case WE.Err(_) => None

        case WE.Node(Program(clss, progb, range)) => 
            for
                clss  <- clss.traverse(classWEToClean)
                progb  <- progBlockWEToClean(progb)
            yield 
                Program[Clean](clss, progb, range)

    // Module (with types) helpers 

    def moduleWEToClean(m: ModuleWE): Option[CleanModule] = m match 
        case WE.Err(_) => None

        case WE.Node(Module(mname, imports, clas, range)) =>
            for 
                mname   <- stringWEToClean(mname)
                imports <- imports.traverse(importToClean)
                clas    <- classWEToClean(clas)
            yield 
                Module[Clean](mname, imports, clas, range)

    def importToClean(i: ImportWE): Option[CleanImport] = i match
        case WE.Err(e) => 
            None
        case WE.Node(Import.Typed(mname, shape, range)) =>
            for
                mname <- stringWEToClean(mname)
                shape <- shapeWEToClean(shape)
            yield
                Import.Typed[Clean](mname, shape, range)
        case WE.Node(Import.Untyped(mname, range)) =>
            untypedImportToClean(WE.Node(Import.Untyped(mname, range)))

    def untypedImportToClean(i: WE[Import.Untyped[WE]]): Option[Clean[Import.Untyped[Clean]]] = i match
        case WE.Err(e) => 
            None
        case WE.Node(Import.Untyped(mname, range)) =>
            for 
                mname <- stringWEToClean(mname)
            yield 
                Import.Untyped[Clean](mname, range)

    def typeWEToClean(typ: TypeWE): Option[CleanType] = typ match
        case WE.Err(_) => None

        case WE.Node(Type.Number(range)) => Some(Type.Number(range))

        case WE.Node(Type.Shape(ftypes, mtypes, range)) => 
            shapeWEToClean(WE.Node(Type.Shape(ftypes, mtypes, range)))

    def shapeWEToClean(s: ShapeTypeWE): Option[CleanShapeType] = s match 
        case WE.Err(_) => None

        case WE.Node(Type.Shape(ftypes, mtypes, range)) =>
            for 
                ftypes <- ftypes.traverse(ftypeWEToClean)
                mtypes <- mtypes.traverse(mtypeWEToClean)
            yield 
                Type.Shape[Clean](ftypes, mtypes, range)

    def ftypeWEToClean(f: FieldTypeWE): Option[CleanFieldType] = f match 
        case WE.Err(_) => None

        case WE.Node(FieldType(fname, ftype, range)) =>
            for 
                fname <- stringWEToClean(fname)
                ftype <- typeWEToClean(ftype)
            yield 
                FieldType[Clean](fname, ftype, range)

    def mtypeWEToClean(m: MethodTypeWE): Option[CleanMethodType] = m match 
        case WE.Err(_) => None

        case WE.Node(MethodType(mname, paramTypes, retType, range)) =>
            for 
                mname      <- stringWEToClean(mname)
                paramTypes <- paramTypes.traverse(typeWEToClean)
                retType    <- typeWEToClean(retType)
            yield 
                MethodType[Clean](mname, paramTypes, retType, range)

    // Class helpers 
        
    def classWEToClean(c: ClassWE): Option[CleanClass] = c match 
        case WE.Err(_) => None

        case WE.Node(Class(cname, fields, methods, Some(shape), range)) =>
            for 
                cname <- stringWEToClean(cname)
                fields <- fields.traverse(stringWEToClean)
                methods <- methods.traverse(methodWEToClean)
                shape <- shapeWEToClean(shape)
            yield 
                Class[Clean](cname, fields, methods, Some(shape), range)

        case WE.Node(Class(cname, fields, methods, None, range)) =>
            for 
                cname <- stringWEToClean(cname)
                fields <- fields.traverse(stringWEToClean)
                methods <- methods.traverse(methodWEToClean)
            yield
                Class[Clean](cname, fields, methods, None, range)


    def methodWEToClean(m: MethodWE): Option[CleanMethod] = m match 
        case WE.Err(_) => None

        case WE.Node(Method(mname, params, progb, range)) =>
            for 
                mname <- stringWEToClean(mname)
                params <- params.traverse(stringWEToClean)
                progb  <- progBlockWEToClean(progb)
            yield
                Method[Clean](mname, params, progb, range)
    
    // Core helpers

    def progBlockWEToClean(pblock : ProgBlockWE) : Option[CleanProgBlock] = pblock match
        case WE.Err(_) => None
        
        case WE.Node(ProgBlock(decls, stmts, expr, range)) => 
            for
                decls <- decls.traverse(declWEToClean)
                stmts <- stmts.traverse(stmtWEToClean)
                expr  <- exprWEToClean(expr)
            yield 
                ProgBlock[Clean](decls, stmts, expr, range)

    def declWEToClean(d: DeclWE): Option[CleanDecl] = d match 
        case WE.Err(_) => None

        case WE.Node(Decl(varDecl, rhs, range)) =>
            for 
                varDecl <- stringWEToClean(varDecl)
                rhs <- exprWEToClean(rhs)
            yield 
                Decl[Clean](varDecl, rhs, range)

    def stmtWEToClean(s: StmtWE): Option[CleanStmt] = s match 
        case WE.Err(_) => None

        case WE.Node(Stmt.Assign(lhs, rhs, range)) => 
            for 
                lhsClean <- stringWEToClean(lhs)
                rhsClean <- exprWEToClean(rhs)
            yield 
                Stmt.Assign[Clean](lhsClean, rhsClean, range)

        case WE.Node(Stmt.Ifelse(guard, tbranch, ebranch, range)) =>
            for
                guardClean <- exprWEToClean(guard)
                tbranchClean <- stmtBlockWEToClean(tbranch)
                ebranchClean <- stmtBlockWEToClean(ebranch)
            yield 
                Stmt.Ifelse[Clean](guardClean, tbranchClean, ebranchClean, range)

        case WE.Node(Stmt.While(guard, body, range)) =>
            for 
                guardClean <- exprWEToClean(guard)
                bodyClean <- stmtBlockWEToClean(body)
            yield
                Stmt.While[Clean](guardClean, bodyClean, range)

        case WE.Node(Stmt.FieldAssign(instance, field, rhs, range)) =>
            for 
                instanceClean <- stringWEToClean(instance)
                fieldClean <- stringWEToClean(field)
                rhsClean <- exprWEToClean(rhs)
            yield 
                Stmt.FieldAssign[Clean](instanceClean, fieldClean, rhsClean, range)
  
    def stmtBlockWEToClean(b: StmtBlockWE): Option[CleanStmtBlock] = b match 
      case WE.Err(_) => None

      case WE.Node(StmtBlock.One(stmt, range)) =>  
        stmtWEToClean(stmt).map(stmtClean => StmtBlock.One[Clean](stmtClean, range))
          
      case WE.Node(StmtBlock.Many(decls, stmts, range)) =>  
        for 
            declsClean <- decls.traverse(declWEToClean)
            stmtsClean <- stmts.traverse(stmtWEToClean)
        yield 
            StmtBlock.Many[Clean](declsClean, stmtsClean, range)
  
    def exprWEToClean(e: ExprWE): Option[CleanExpr] = e match 
        case WE.Err(_) => None

        case WE.Node(Expr.Num(n, range)) => 
            Some(Expr.Num[Clean](n, range))
            
        case WE.Node(Expr.Var(x, range)) => 
            stringWEToClean(x).map(v => Expr.Var[Clean](v, range))

        case WE.Node(Expr.BinOpExpr(lhs, op, rhs, range)) => 
            for 
                lhsClean <- stringWEToClean(lhs)
                rhsClean <- stringWEToClean(rhs)
            yield 
                Expr.BinOpExpr[Clean](lhsClean, op, rhsClean, range)
            
        case WE.Node(Expr.NewInstance(cname, args, range)) => 
            for 
                cnameClean <- stringWEToClean(cname)
                argsClean <- args.traverse(stringWEToClean)
            yield
                Expr.NewInstance[Clean](cnameClean, argsClean, range)
            
        case WE.Node(Expr.GetField(instance, field, range)) => 
            for
                instanceClean <- stringWEToClean(instance)
                fieldClean <- stringWEToClean(field)
            yield 
                Expr.GetField[Clean](instanceClean, fieldClean, range)
            
        case WE.Node(Expr.CallMethod(instance, method, args, range)) => 
            for 
                instanceClean <- stringWEToClean(instance)
                methodClean <- stringWEToClean(method)
                argsClean <- args.traverse(stringWEToClean)
            yield 
                Expr.CallMethod[Clean](instanceClean, methodClean, argsClean, range)
            
        case WE.Node(Expr.IsInstanceOf(instance, cname, range)) => 
            for 
                instanceClean <- stringWEToClean(instance)
                cnameClean <- stringWEToClean(cname)
            yield 
                Expr.IsInstanceOf[Clean](instanceClean, cnameClean, range)

    def stringWEToClean(v: WE[String]): Option[Clean[String]] = v match 
        case WE.Err(_) => None
        case WE.Node(vr) => Some(vr)
