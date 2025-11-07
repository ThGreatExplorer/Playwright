package static

import ast._
import ast.TypeErrorNodes._
import ast.Type.Shape
import util.{getCNames}
import util.getFTypeNames
import util.getMTypeNames

// object Typechecker:

object TypeChecker:

    // Top Level entry points

    def typeCheckSystem(s: CleanSystem): SystemWE = s match
        case System[Clean](modules, imports, progb) =>
            val (validatedModules, modToCNameAndShapeMapSoFar) = typeCheckModules(modules)
            // Classes in scope constructed based on the sequence of modules 
            val sClassesMap = typeCheckImports(imports, modToCNameAndShapeMapSoFar)
            // Variables obey lexical scope, so we begin with an empty Map from variables to Types
            val tVars = Map[String, CleanType]()
            val (optEType, progWE) = typeCheckProgb(progb, sClassesMap, tVars)
            optEType match
                case None => 
                    WE.Node(System(
                        validatedModules,
                        imports.map(ConverterToWE.stringToWE(_)),
                        progWE
                    ))
                case Some(returnType) =>
                    returnType match
                        case Type.Number() => 
                            WE.Node(System(
                                validatedModules,
                                imports.map(ConverterToWE.stringToWE(_)),
                                progWE
                            ))
                        case Shape(fieldTypes, methodTypes) =>
                            WE.Err(TypeErrorNodes.TopLevelReturnNotANumber)

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
                                _ <- Either.cond(methods.lengthIs == methodTypes.length, (),
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
                                                val tVars: Map[String, CleanType] = Map("this" -> expectedShape)
                                                val (_, methodWE) = typeCheckMethodWithMethodType(method, methodType, sClassesMap ,tVars)
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
        
    def typeCheckMethodWithMethodType(m: CleanMethod, mtype: CleanMethodType, sClassesMap: Map[String, CleanShapeType], tVars: Map[String, CleanType]): (Option[CleanType], MethodWE) = m match
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
                            val updatedTVars: Map[String, CleanType] = tVars ++ params.zip(paramTypes).toMap
                            val (optProgRType, progBWE) = typeCheckProgb(progb, sClassesMap, updatedTVars)
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
                    stmts.map(typecheckStmt(_, sClassesMap, updatedTVarsdecls)),
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
                            givenFType == fieldType.fieldType
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
                tVars(instance) match
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
                                        

    def typecheckStmt(stmt: CleanStmt, sClassesMap: Map[String, CleanShapeType], tVars: Map[String, CleanType]): StmtWE = stmt match
        case Stmt.Assign(id, rhs) => 
            val (optType, exprWE) = typeCheckExpr(rhs, sClassesMap, tVars)
            optType match
                case None => 
                    WE.Node(Stmt.Assign(
                        WE.Node(id), 
                        exprWE
                    ))
                case Some(exprType) => 
                    if exprType == tVars(id) then
                        WE.Node(Stmt.Assign(
                            WE.Node(id), 
                            exprWE
                        ))
                    else 
                        WE.Err(StrongTypingViolation)

        case Stmt.Ifelse(guard, tbranch, ebranch) =>
            val (_, exprWE) = typeCheckExpr(guard, sClassesMap, tVars)
            WE.Node(Stmt.Ifelse(
                exprWE, 
                typecheckSBlock(tbranch, sClassesMap, tVars), 
                typecheckSBlock(ebranch, sClassesMap, tVars) 
            ))                   
            
        case Stmt.While(guard, body) =>
            val (_, exprWE) = typeCheckExpr(guard, sClassesMap, tVars)
            WE.Node(Stmt.While(
                exprWE, 
                typecheckSBlock(body, sClassesMap, tVars)
            )) 

        case Stmt.FieldAssign(instance, fname, rhs) =>
            val (optType, exprWE) = typeCheckExpr(rhs, sClassesMap, tVars)
            optType match
                case None => 
                    WE.Node(Stmt.FieldAssign(
                        WE.Node(instance),
                        WE.Node(fname),
                        exprWE
                    ))
                case Some(exprType) => 
                    tVars(instance) match
                        case Type.Number() => 
                            WE.Err(TypeErrorNodes.InstanceIsNotAShape)
                        case instShape @ Shape(fieldTypes, methodTypes) =>
                            val (optExpectedFtype, fnameWE) = ShapeUtils.getFieldType(instShape, fname)
                                optExpectedFtype match
                                    case None => 
                                        WE.Node(Stmt.FieldAssign(
                                            WE.Node(instance),
                                            fnameWE,
                                            exprWE
                                        ))
                                    case Some(expectedFtype) => 
                                        if exprType == expectedFtype then
                                            WE.Node(Stmt.FieldAssign(
                                                WE.Node(instance),
                                                fnameWE,
                                                exprWE
                                            ))
                                        else 
                                            WE.Err(StrongTypingViolation)
                    
    def typecheckSBlock(b: CleanStmtBlock, sClassesMap: Map[String, CleanShapeType], tVars: Map[String, CleanType]) : StmtBlockWE = b match
        case StmtBlock.One(stmt) => 
            WE.Node(StmtBlock.One(
                typecheckStmt(stmt, sClassesMap, tVars)
            ))

        case StmtBlock.Many(decls, stmts) => 
            val (updatedTVarsdecls, declsWE) = decls.foldLeft((tVars, List[DeclWE]())){
                    case ((tVarAcc, declList), decl) =>
                        val (optTVar, declWE) = typeCheckDecl(decl, sClassesMap, tVarAcc)
                        optTVar match
                            case None => (tVarAcc, declList :+ declWE)
                            case Some((varDecl, vType)) => (tVarAcc.updated(varDecl, vType), declList :+ declWE)
                }

            WE.Node(StmtBlock.Many(
                declsWE, 
                stmts.map(typecheckStmt(_, sClassesMap, updatedTVarsdecls))
            ))           
    
object ShapeUtils:

    def getFieldType(s : CleanShapeType, targetFName: String) : (Option[CleanType], NameWE) = s match
        case Type.Shape(fieldTypes, methodTypes) =>
            val optTargetFtype = fieldTypes.find(ftype => ftype.fname == targetFName)

            optTargetFtype match
                case Some(FieldType(fname, typ)) => 
                    (Some(typ), WE.Node(fname))
                case None =>
                    (None, WE.Err(TypeErrorNodes.FieldDoesNotExist))

    def getMethodType(s : CleanShapeType, targetMName: String) : (Option[(List[CleanType], CleanType)], NameWE) = s match
        case Type.Shape(fieldTypes, methodTypes) =>
            val optTargetMtype = methodTypes.find(mtype => mtype.mname == targetMName)

            optTargetMtype match
                case Some(MethodType(mname, paramTypes, retType)) => 
                    (Some((paramTypes, retType)), WE.Node(mname))
                case None =>
                    (None, WE.Err(TypeErrorNodes.CallMethodDoesNotExist))