package static

import ast._
import ast.ValidityErrNodes._
import ast.Type.Shape
import static.ShapeUtils.matchShapeWithClass
import util.{getCNames}

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
                val processedModule = WE.Node(Module(
                    WE.Node(mname),
                    imports,
                    closedClass(clas, updatedSClassesMap),
                    shape.map(ConverterToWE.shapeToWE)
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
     * @return Tuple of validated Import nodes and Map from Class Names to their Shapes 
     */ 
    def typeCheckImports(
        imports: List[CleanImportedMod], modToCNameAndShapeMap : Map[String, (String, CleanShapeType)]
    ) : Map[String, CleanShapeType] =
        val modsInScope      = modToCNameAndShapeMap.keySet
        // val validatedImports = imports.map(closedImportedModName(_, modsInScope))
        val sClassesMap      = imports.foldLeft(Map[String, (String, CleanShapeType)]()){
                case (acc, WE.Err(_)) => acc
                case (acc, importedMod) =>
                    val (cname, shape) = modToCNameAndShapeMap(importedMod) 
                    acc.updated(importedMod, (cname, shape))
            }.values.toMap

        sClassesMap

    def closedImportedModName(c: CleanImportedMod, modsInScope: Set[String]): ImportedModWE =
        if modsInScope.contains(c) then
            WE.Node(c)
        else
            WE.Err(ModuleNotDeclared) 

    // Class helpers

    def typeCheckClass(cls : CleanClass, sClassesMap: Map[String, CleanShapeType]) : ClassWE = 
        val expectedShape = sClassesMap(cls.cname)
        cls match
            case Class(cname, fields, methods) => 
                expectedShape match
                    case Shape(fieldTypes, methodTypes) =>
                        val result = for
                            matchingLengths <- Option.when(fields.lengthIs == fieldTypes.length && methodTypes.lengthIs == methods.length)(())
                            matchingFields <- Option.when(fields.sorted.zip(fieldTypes.sortBy(_.fname)).forall { 
                                case (field, fieldType) =>
                                    field == fieldType.fname
                            })(())
                            matchingMethods <- Option.when(methods.sortBy(_.mname).zip(methodTypes.sortBy(_.mname)).forall{
                                case (method, methodType) =>
                                    matchMethodTypeWithMethodDef(method, methodType, sClassesMap)
                            })(())
                        yield 
                            true
                        result.getOrElse(false)

    def matchMethodTypeWithMethodDef(m: CleanMethod, mtype: CleanMethodType, sClassesMap: Map[String, CleanShapeType]): Boolean = m match
        case Method(mname, params, progb) =>
            mtype match
                case MethodType(mtname, paramTypes, returnType) =>
                    if mname != mtname then
                        false
                    else 
                        if params.lengthIs != paramTypes.length then
                            false
                        else
                            val tVars: Map[String, CleanType] = params.zip(paramTypes).toMap
                            typeCheckProgb(progb, sClassesMap, tVars)

    def typeCheckProgb(progb: CleanProgBlock, sClassesMap: Map[String, CleanShapeType], tVars: Map[String, CleanType]): Boolean =
        progb match
            case ProgBlock(decls, stmts, expr) =>  


    def typeCheckDecl(decl: CleanDecl, sClassesMap: Map[String, CleanShapeType], tVars: Map[String, CleanType]): (Map[String,CleanType], Boolean) =
        decl match
            case Decl(varDecl, rhs) =>
                typeCheckExpr(rhs, sClassesMap, tVars)
                val updatedTVars = tVars.updated(varDecl,)
                (updatedTVars, )
    
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
            case Expr.GetField()
        

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
                    val instShape = tVars(instance)
                    val (optExpectedFtype, fnameWE) = getFieldType(instShape, fname)

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
    
    def generateShapeFromClass(c: CleanClass): ShapeTypeWE = c match
        case Class(_, fields, methods) =>
            WE.Node(Shape(
                fields.map(generateFieldTypeFromField),
            ))

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