package static

import ast._
import ast.ValidityErrNodes._
import ast.Type.Shape
import static.ShapeUtils.matchShapeWithClass
import util.{getCNames}
import util.getFTypeNames
import util.getMTypeNames

// object Typechecker:

object TypeChecker:

    // Top Level entry points

    def typeCheckSystem(s: CleanSystem): SystemWE = WE.Node(s match
        case System[Clean](modules, imports, progb) =>
            val (validatedModules, modToCNameMap) = closedModules(modules)
            // Classes in scope constructed based on the sequence of modules 
            val (validatedImports, clssInScope)   = closedImports(imports, modToCNameMap)
            // Variables obey lexical scope, so we begin with an empty environment
            val varsInScope = Set[String]()

            System(
                validatedModules,
                validatedImports,
                closedProgBlock(progb, clssInScope, varsInScope)
            )
        )

    // Module helpers

    /**
    * Validates that each module is closed with respect to collection of imported classes.
    * During validation, constructs a Map from ModuleNames to ClassNames that were defined
    * in this list of module definitions.
    *
    * @param mods List of CleanModule to be processed
    * @return Tuple of validated Module nodes and Map[ModuleName, ClassName] 
    */
    def typeCheckModules(mods: List[CleanModule]) : (List[ModuleWE], Map[String, (String, CleanShapeType)]) = 

        def typeCheckModulesLoop(
            modsRem: List[CleanModule], modsSoFar: List[ModuleWE], modToCNameAndShapeMapSoFar : Map[String, (String, CleanShapeType)]
        ) : (List[ModuleWE], Map[String, (String, CleanShapeType)]) = modsRem match

            case Nil => (modsSoFar.reverse, modToCNameAndShapeMapSoFar)

            case Module(mname, imports, clas, shape) :: tail => 
                val sClassesMap = typeCheckImports(imports, modToCNameAndShapeMapSoFar)
                val (updatedSClassesMap, updModToCNameAndShapeMapSoFar) = shape match
                    case None => (sClassesMap, modToCNameAndShapeMapSoFar)
                    case Some(actualShape) => (sClassesMap.updated(clas.cname, actualShape), modToCNameAndShapeMapSoFar.updated(mname, (clas.cname, actualShape)))
                val (optShapeType, clssWE) = typeCheckClass(clas, updatedSClassesMap)
                val processedModule = WE.Node(Module(
                    WE.Node(mname),
                    imports.map(ConverterToWE.stringToWE(_)),
                    clssWE,
                    shape.map(ConverterToWE.shapeToWE) // TODO what does this really mean to convert to shapeWE? Should typeCheckClass produce a ShapeWE instead of Option[CleanShape]
                ))

                typeCheckModulesLoop(tail, processedModule :: modsSoFar, updModToCNameAndShapeMapSoFar)

        typeCheckModulesLoop(mods, Nil, Map[String, (String, CleanShapeType)]())

    /**
     * Constructs the sClasses Map from class name to Shape of all the classes that are in scope for
     * the scope of this sequence of imports.
     * 
     * @param imports List of imports 
     * @param modToCNameAndShapeMap Map from ModuleNames to (ClassNames, ClassShapes) that were define
     * before this sequence of imports
     * @return Map from Class Names to their Shapes 
     */ 
    def typeCheckImports(
        imports: List[CleanImportedMod], modToCNameAndShapeMap : Map[String, (String, CleanShapeType)]
    ) : Map[String, CleanShapeType] =
        val modsInScope      = modToCNameAndShapeMap.keySet
        val sClassesMap      = imports.foldLeft(Map[String, (String, CleanShapeType)]()){
                case (acc, WE.Err(_)) => acc
                case (acc, importedMod) =>
                    val (cname, shape) = modToCNameAndShapeMap(importedMod) 
                    acc.updated(importedMod, (cname, shape))
            }.values.toMap

        sClassesMap

   
    // Class helpers

    def typeCheckClass(cls : CleanClass, sClassesMap: Map[String, CleanShapeType]) : (Option[CleanShapeType] , ClassWE) = 
        val expectedShape = sClassesMap(cls.cname)
        cls match
            case Class(cname, fields, methods) => 
                expectedShape match
                    case Shape(fieldTypes, methodTypes) =>
                        val result = 
                            for
                                _ <- Either.cond(fields.lengthIs == fieldTypes.length, (), 
                                    WE.Err(TypeErrorNodes.ShapeTypeWrongNumberOfFields))
                                _ <- Either.cond(methods.length == methodTypes.length, (),
                                    WE.Err(TypeErrorNodes.ShapeTypeWrongNumberOfMethods))
                                _ <- Either.cond(
                                        fields.sorted.zip(fieldTypes.sortBy(_.fname)).forall { 
                                            case (field, fieldType) =>
                                                field == fieldType.fname
                                        }, 
                                        (), 
                                        WE.Err(TypeErrorNodes.ShapeTypeFieldTypeMismatch))
                            yield 
                                val methodsWE = methods.sortBy(_.mname).zip(methodTypes.sortBy(_.mname)).map{
                                            case (method, methodType) =>
                                                val (_, methodWE) = typeCheckMethodWithMethodType(method, methodType, sClassesMap)
                                                methodWE
                                        }
                                WE.Node(Class(
                                    ConverterToWE.stringToWE(cname),
                                    fields.map(ConverterToWE.stringToWE(_)),
                                    methodsWE
                                ))
                        
                        result match
                            case Left(errorNode) => (None, errorNode)
                            case Right(validClass) => (Some(expectedShape), validClass)
        
    def typeCheckMethodWithMethodType(m: CleanMethod, mtype: CleanMethodType, sClassesMap: Map[String, CleanShapeType]): (Option[CleanType], MethodWE) = m match
        case Method(mname, params, progb) =>
            mtype match
                case MethodType(mtname, paramTypes, returnType) =>
                    // TODO should these comparisons be here or in the typeCheckClasses
                    if mname != mtname then
                        (None, WE.Err(TypeErrorNodes.ShapeTypeMethodNameMismatch))
                    else 
                        if params.lengthIs != paramTypes.length then
                            (None, WE.Err(TypeErrorNodes.ShapeTypeMethodWrongNumberOfParams))
                        else
                            val tVars: Map[String, CleanType] = params.zip(paramTypes).toMap
                            val (optProgRType, progBWE) = typeCheckProgb(progb, sClassesMap, tVars)
                            optProgRType match
                                case None => 
                                    (None, WE.Node(Method(
                                        ConverterToWE.stringToWE(mname),
                                        params.map(ConverterToWE.stringToWE(_)),
                                        progBWE
                                    )))
                                case Some(progReturnType) =>
                                    progReturnType == returnType match
                                        case false => 
                                            (None, WE.Err(TypeErrorNodes.ProgBlockReturnWrongType))
                                        case true =>
                                            (Some(returnType), WE.Node(Method(
                                                ConverterToWE.stringToWE(mname),
                                                params.map(ConverterToWE.stringToWE(_)),
                                                progBWE
                                            )))
                            

    def typeCheckProgb(progb: CleanProgBlock, sClassesMap: Map[String, CleanShapeType], tVars: Map[String, CleanType]): (Option[CleanType], ProgBlockWE) =
        progb match
            case ProgBlock(decls, stmts, expr) => 
                val (updatedTVarsdecls, declsWE) = decls.foldLeft((tVars, List[DeclWE]())){
                    case ((tVarAcc, declList), decl) =>
                        val (optTVar, declWE) = typeCheckDecl(decl, sClassesMap, tVarAcc)
                        optTVar match
                            case None => (tVarAcc, declList :+ declWE)
                            case Some((varDecl, vType)) => (tVarAcc.updated(varDecl, vType), declList :+ declWE)
                }

                val (optEType, exprWE) = typeCheckExpr(expr, sClassesMap, updatedTVarsdecls)
                
                (optEType, WE.Node(ProgBlock(
                    declsWE,
                    stmtsWE,
                    exprWE
                )))
                


    def typeCheckDecl(decl: CleanDecl, sClassesMap: Map[String, CleanShapeType], tVars: Map[String, CleanType]): (Option[(String,CleanType)], DeclWE) =
        decl match
            case Decl(varDecl, rhs) =>
                val (optType, exprWE) = typeCheckExpr(rhs, sClassesMap, tVars)
                optType match
                    case None => 
                        (None, WE.Node(Decl(
                            ConverterToWE.stringToWE(varDecl), 
                            exprWE
                        )))
                    case Some(eType) => 
                        (Some(varDecl, eType), WE.Node(Decl(
                            ConverterToWE.stringToWE(varDecl), 
                            exprWE
                        )))
    
    def typeCheckExpr(expr: CleanExpr, sClassesMap: Map[String, CleanShapeType], tVars: Map[String, CleanType]): (Option[CleanType], ExprWE) = 
        expr match
            case Expr.Num(n) => 
                (Some(Type.Number()), WE.Node(Expr.Num(n)))
            case Expr.Var(varRef) =>
                (Some(tVars(varRef)), WE.Node(Expr.Var(ConverterToWE.stringToWE(varRef))))
            case Expr.BinOpExpr(lhs, op, rhs) =>
                val lhsType = tVars(lhs)
                val rhsType = tVars(rhs)
                op match
                    case BinOp.Equals => 
                        (Some(Type.Number()), WE.Node(Expr.BinOpExpr(
                            ConverterToWE.stringToWE(lhs),
                            op,
                            ConverterToWE.stringToWE(rhs)
                        )))
                    case _ =>
                        (lhsType, rhsType) match
                            case (Type.Number(), Type.Number()) =>
                                (Some(Type.Number()), WE.Node(Expr.BinOpExpr(
                                    ConverterToWE.stringToWE(lhs),
                                    op,
                                    ConverterToWE.stringToWE(rhs)
                                )))
                            case (_, _) =>
                                (None, WE.Err(TypeErrorNodes.BinOpWithNonNumberType))
            case Expr.GetField(instance, fname) =>
                tVars(instance) match
                    case Type.Number() => 
                        (None, WE.Err(TypeErrorNodes.GetFieldCalledOnNonShapeType))
                    case Shape(fieldTypes, methodTypes) => 
                        fieldTypes.zip(fieldTypes.getFTypeNames).find((_, targetFName) => targetFName == fname) match
                            case Some(fieldType, _) => 
                                (Some(fieldType.fieldType), WE.Node(Expr.GetField(
                                    ConverterToWE.stringToWE(instance),
                                    ConverterToWE.stringToWE(fname)
                                ))) 
                            case None =>
                                (None, WE.Err(TypeErrorNodes.FieldDoesNotExist))                        
            case Expr.IsInstanceOf(instance, cname) =>
                tVars(instance) match
                    case Type.Number() => 
                        (None, WE.Err(TypeErrorNodes.IsACalledWithNonNumberType))
                    case expectedShape @ Shape(fieldTypes, methodTypes) =>
                        expectedShape == sClassesMap(cname) match
                            case false =>
                                (None, WE.Err(TypeErrorNodes.IsAShapeMismatch))
                            case true =>
                                (Some(expectedShape), WE.Node(Expr.IsInstanceOf(
                                    ConverterToWE.stringToWE(instance),
                                    ConverterToWE.stringToWE(cname)
                                )))
            case Expr.NewInstance(cname, args) =>
                val expectedShape = sClassesMap(cname)
                args.lengthIs == expectedShape.fieldTypes.length match
                    case false => 
                        (None, WE.Err(TypeErrorNodes.NewInstanceWrongNumberOfFields))
                    case true =>
                        // TODO, this same logic is used in matchingClass function
                        val matchingFields = args.zip(expectedShape.fieldTypes).forall((arg, fieldType) => 
                            val givenFType = tVars(arg)
                            givenFType == fieldType
                        )
                        matchingFields match
                            case false =>
                                (None, WE.Err(TypeErrorNodes.NewInstanceFieldWrongType))
                            case true =>
                                (Some(expectedShape), WE.Node(Expr.NewInstance(
                                    ConverterToWE.stringToWE(cname),
                                    args.map(ConverterToWE.stringToWE(_))
                                )))
            case Expr.CallMethod(instance, method, args) =>
                tVars(method) match
                    case Type.Number() => 
                        (None, WE.Err(TypeErrorNodes.CallMethodWithNonNumberType))
                    case Shape(fieldTypes, methodTypes) =>
                        // TODO, similar to finding the fieldName
                        methodTypes.zip(methodTypes.getMTypeNames).find((methodType, mname) =>
                        mname == method) match
                            case None =>
                                (None, WE.Err(TypeErrorNodes.CallMethodDoesNotExist))
                            case Some((methodType, _)) =>
                                args.lengthIs == methodType.paramTypes.length match
                                    case false =>
                                        (None, WE.Err(TypeErrorNodes.CallMethodWrongNumberOfParams))
                                    case true =>
                                        // TODO, similar logic as in matchingClass function
                                        val matchingParams = args.zip(methodType.paramTypes).forall(
                                            (arg, paramType) =>
                                                val givenParamType = tVars(arg)
                                                givenParamType == paramType
                                        )
                                        matchingParams match
                                            case false => 
                                                (None, WE.Err(TypeErrorNodes.CallMethodParamWrongType))
                                            case true =>
                                                (Some(methodType.returnType), WE.Node(Expr.CallMethod(
                                                    ConverterToWE.stringToWE(instance),
                                                    ConverterToWE.stringToWE(method),
                                                    args.map(ConverterToWE.stringToWE(_))
                                                )))
                                        

    def closedMethod(mth : CleanMethod, clssInScope: Set[String]) : MethodWE = WE.Node( mth match
        case Method(mname, params, pblock) => 
            // Parameters are implicitly declared before the method body with "this"
            val varsInScope : Set[String] = ("this" :: params).toSet

            Method(
                WE.Node(mname),
                params.map(WE.Node(_)),
                closedProgBlock(pblock, clssInScope, varsInScope)
            )
        )

    // Core helpers

    def closedProgBlock(pb: CleanProgBlock, clssInScope: Set[String], varsInScope: Set[String]) : ProgBlockWE = WE.Node( pb match
        case ProgBlock(decls, stmts, expr) => 
            val (validatedDecls, extVarsInScope) = closedDecls(decls, clssInScope, varsInScope)

            ProgBlock(
                validatedDecls,
                stmts.map(closedStmt(_,  clssInScope, extVarsInScope)),
                closedExpr(expr, clssInScope, extVarsInScope)
            )
    )

    /**
    * Accumulator that accumulates the Decls and Variables processed so far,
    * evaluating the first declaration, updating the variables, and 
    * appending the declarations processed before recursing.
    *
    * @param decls LIst of CleanDecl to be processed
    * @param clssInScope Set of classes declared in the program
    * @param varsInScope Set of variables declared so far
    * @return Tuple of List[DeclWE], Set[String]
    */
    def closedDecls(decls: List[CleanDecl], clssInScope: Set[String], varsInScope: Set[String]) : (List[DeclWE], Set[String]) = 

        def closedDeclsLoop(declsRem: List[CleanDecl], declsSoFar: List[DeclWE], dvarsSoFar: Set[String]) 
        : (List[DeclWE], Set[String]) = declsRem match

            case Nil => (declsSoFar.reverse, dvarsSoFar)

            case Decl(name, rhs) :: tail => 
                val processedDecl = WE.Node(Decl(
                    WE.Node(name),
                    closedExpr(rhs, clssInScope, dvarsSoFar)
                ))

                closedDeclsLoop(tail, processedDecl :: declsSoFar, dvarsSoFar.incl(name))

        closedDeclsLoop(decls, Nil, varsInScope)

    def closedStmt(stmt: CleanStmt, clssInScope : Set[String], varsInScope: Set[String]): StmtWE = WE.Node( stmt match
        case Stmt.Assign(id, rhs) => 
            Stmt.Assign(
                closedVarRef(id, varsInScope), 
                closedExpr(rhs, clssInScope, varsInScope)
            )

        case Stmt.Ifelse(guard, tbranch, ebranch) =>
            Stmt.Ifelse(
                closedExpr(guard, clssInScope, varsInScope), 
                closedSBlock(tbranch, clssInScope, varsInScope), 
                closedSBlock(ebranch, clssInScope, varsInScope) 
            )

        case Stmt.While(guard, body) =>
            Stmt.While(
                closedExpr(guard, clssInScope, varsInScope),
                closedSBlock(body, clssInScope, varsInScope)
            )

        case Stmt.FieldAssign(instance, fname, rhs) =>
            Stmt.FieldAssign(
                closedVarRef(instance, varsInScope), 
                WE.Node(fname),
                closedExpr(rhs, clssInScope, varsInScope)
            )
        )
        
    def closedSBlock(b: CleanStmtBlock, clssInScope : Set[String], varsInScope: Set[String]): StmtBlockWE = WE.Node( b match
        case StmtBlock.One(stmt) => 
            StmtBlock.One(closedStmt(stmt, clssInScope, varsInScope))

        case StmtBlock.Many(decls, stmts) => 
            val (processedDecls, extVarsInScope) = closedDecls(decls, clssInScope, varsInScope)
            StmtBlock.Many(
                processedDecls, 
                stmts.map(closedStmt(_, clssInScope, extVarsInScope))
            )
        )
            
    def closedExpr(e: CleanExpr, clssInScope : Set[String], varsInScope: Set[String]): ExprWE = WE.Node(e match
        case Expr.Num(n) => Expr.Num(n)

        case Expr.Var(x) => 
            Expr.Var(closedVarRef(x, varsInScope))

        case Expr.BinOpExpr(lhs, op, rhs) => 
            Expr.BinOpExpr(
                closedVarRef(lhs, varsInScope), 
                op,
                closedVarRef(rhs, varsInScope)
            )
        
        case Expr.NewInstance(cname, args) =>
            Expr.NewInstance(
                closedClassNameRef(cname, clssInScope),
                args.map(closedVarRef(_, varsInScope))
            )

        case Expr.IsInstanceOf(instance, cname) =>
            Expr.IsInstanceOf(
                closedVarRef(instance, varsInScope),
                closedClassNameRef(cname, clssInScope)
            )

        case Expr.GetField(instance, fname) =>
            Expr.GetField(
                closedVarRef(instance, varsInScope),
                WE.Node(fname)
            )
        
        case Expr.CallMethod(instance, mname, args) =>
            Expr.CallMethod(
                closedVarRef(instance, varsInScope),
                WE.Node(mname),
                args.map(closedVarRef(_, varsInScope))
            )
        
    )

    def closedVarRef(v: CleanVarRef, varsInScope: Set[String]): VarRefWE  = 
        if varsInScope.contains(v) then
            WE.Node(v)
        else
            WE.Err(VarNotDeclared) 
    
    def closedClassNameRef(c: CleanName, clssInScope: Set[String]): NameWE =
        if clssInScope.contains(c) then
            WE.Node(c)
        else
            WE.Err(ClassNotDeclared) 

    // def generateClassesToShapeMap(): Map[String, ShapeTypeWE] = s match
    //     case System(modules, imports, progb) => 
            
    /* Using Module Dependency DAG */
    // def typeCheckSys(s: CleanSystem): SystemWE = s match
    //     case System(modules, imports, progb) => 
    //         val baseModule = SystemToClassLinker.generateBaseModule(modules, imports) 
    //         val sClassesMap = baseModule.generateSClassesMap()
    //         WE.Node(System(
    //             typeCheckModules(modules, baseModule),
    //             imports.map(ConverterToWE.stringToWE),
    //             typeCheckProgb(progb, sClassesMap)
    //         ))

    // def typeCheckModules(modules: List[CleanModule], baseModule: ModuleDependency): List[ModuleWE] = modules.map(
    //     module => module match
    //         case Module(mname, imports, clas, shape)
        
    // )


    // def typeCheckProgb(p: CleanProgBlock, sClassesMap: Map[String, CleanShapeType]): ProgBlockWE = p match
    //     case ProgBlock(decls, stmts, expr) => 


    
object ShapeUtils:
    