package static

import ast._
import ast.TypeErrorNodes._
import ast.Type.Shape
import util.{getCNames, getFTypeNames, getMTypeNames}
import util.UnreachablePatternMatch

// object Typechecker:

object Typechecker:

    // Top Level entry point

    def typecheckSystem(s: CleanSystem): SystemWE = WE.Node( s match
        case System[Clean](modules, imports, progb) =>

            val (typecheckedModules, modToCNameAndShapeMap) = typecheckModules(modules)
            // Classes in scope constructed based on the sequence of modules 
            val (typecheckedImports, sClasses) = typecheckImports(imports, modToCNameAndShapeMap)
            // Variables obey lexical scope, so we begin with an empty Map from variables to Types
            val tVars = Map[String, CleanType]()

            val topLevelExpectedReturnType : CleanType = Type.Number()

            System(
                typecheckedModules,
                typecheckedImports,
                typecheckProgBWithExpType(progb, topLevelExpectedReturnType, sClasses, tVars)
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
    def typecheckModules(mods: List[CleanModule]) : (List[ModuleWE], Map[String, (String, CleanShapeType)]) = 

        def typecheckModulesLoop(
            modsRem: List[CleanModule], modsSoFar: List[ModuleWE], modToCNameAndShapeMapSoFar : Map[String, (String, CleanShapeType)]
        ) : (List[ModuleWE], Map[String, (String, CleanShapeType)]) = modsRem match

            case Nil => (modsSoFar.reverse, modToCNameAndShapeMapSoFar)

            case Module(mname, imports, clas, shape) :: tail => 
                val (typecheckedImports, sClasses) = typecheckImports(imports, modToCNameAndShapeMapSoFar)

                val (updatedSClasses, updModToCNameAndShapeMapSoFar) = shape match
                    case None => 
                        // Probably use this for next assignment:
                        // (sClasses, modToCNameAndShapeMapSoFar)
                        throw new UnreachablePatternMatch(
                            "Should not happen in A9: module " + mname + " is untyped"
                        )

                    case Some(actualShape) => 
                        (
                            sClasses.updated(clas.cname, actualShape), 
                            modToCNameAndShapeMapSoFar.updated(mname, (clas.cname, actualShape))
                        )
                
                val processedModule = WE.Node(Module(
                    WE.Node(mname),
                    typecheckedImports,
                    typecheckClass(clas, updatedSClasses),
                    shape.map(ConverterToWE.shapeToWE) // TODO refine data representation so that we don't do this
                ))

                typecheckModulesLoop(tail, processedModule :: modsSoFar, updModToCNameAndShapeMapSoFar)

        typecheckModulesLoop(mods, Nil, Map[String, (String, CleanShapeType)]())

    /**
     * Constructs the sClasses Map from class name to Shape of all the classes that are in scope for
     * the scope of this sequence of imports.
     * 
     * @param imports List of imports 
     * @param modToCNameAndShapeMap Map from ModuleNames to (ClassNames, ClassShapes) that were define
     * before this sequence of imports
     * @return Tuple of ImportWE and Map from Class Names to their Shapes 
     */ 
    def typecheckImports(
        imports: List[CleanImportedMod], modToCNameAndShapeMap : Map[String, (String, CleanShapeType)]
    ) : (List[ImportedModWE], Map[String, CleanShapeType])  =

        val sClasses      = imports.foldLeft(Map[String, (String, CleanShapeType)]()){
                case (acc, importedMod) =>
                    val (cname, shape) = modToCNameAndShapeMap(importedMod) 
                    acc.updated(importedMod, (cname, shape))
            }.values.toMap

        val importsWE = imports.map(WE.Node(_))
        (importsWE, sClasses)

   
    // Class helpers

    def typecheckClass(cls : CleanClass, sClasses: Map[String, CleanShapeType]) : ClassWE = 
        val thisShape = sClasses(cls.cname)
        (cls, thisShape) match
            case (Class(_, fields, _), Shape(fieldTypes, _)) if fields.lengthIs != fieldTypes.length => 
                WE.Err(ShapeTypeWrongNumberOfFields)

            case (Class(_, _, methods), Shape(_, methodTypes)) if methods.lengthIs != methodTypes.length => 
                WE.Err(ShapeTypeWrongNumberOfMethods) 

            case (Class(cname, fields, methods), Shape(fieldTypes, methodTypes)) => 
                // According to simplification, we expect fields to appear in the same order
                val fieldsWE = fields.zip(fieldTypes).map{
                        case (fnameInDef, FieldType(fnameInTyp, _)) if fnameInDef != fnameInTyp => 
                            WE.Err(ShapeTypeFieldTypeMismatch)
                        case (fname, _) => 
                            WE.Node(fname)
                    }

                // Since the simplification only applies to fields and not methods,
                // we need to enforce order before comparison
                val orderedMethodDefs = methods.sortBy(_.mname)
                val orderedMethodTypes = methodTypes.sortBy(_.mname)
                val methodsWE = orderedMethodDefs.zip(orderedMethodTypes).map{
                        case (method, methodType) =>
                            typecheckMethodWithMethodType(method, methodType, sClasses, thisShape)
                    }
                
                WE.Node(Class(
                    WE.Node(cname),
                    fieldsWE,
                    methodsWE
                ))

    def typecheckMethodWithMethodType(
        m: CleanMethod, expectedMtype: CleanMethodType, sClasses: Map[String, CleanShapeType], tVarThis : CleanType
    ): MethodWE = (m, expectedMtype) match
        case (Method(mname, _, _), MethodType(mtname, _, _)) if mname != mtname =>
            WE.Err(ShapeTypeMethodNameMismatch)

        case (Method(_, params, _), MethodType(_, paramTypes, _)) if params.lengthIs != paramTypes.length =>
            WE.Err(ShapeTypeMethodWrongNumberOfParams)

        case (Method(mname, params, progb), MethodType(_, paramTypes, retType)) =>
            val paramTVars: Map[String, CleanType] = params.zip(paramTypes).toMap
            val initTVars = paramTVars.updated("this", tVarThis)

            WE.Node(Method(
                WE.Node(mname),
                params.map(WE.Node(_)),
                typecheckProgBWithExpType(progb, retType, sClasses, initTVars)
            ))
                            
    // Core helpers

    def typecheckProgBWithExpType(
        progb: CleanProgBlock, expRetType : CleanType, sClasses: Map[String, CleanShapeType], tVars: Map[String, CleanType]
    ): ProgBlockWE = WE.Node(progb match
        case ProgBlock(decls, stmts, expr) =>

            val (typecheckedDecls, extTVars) = typecheckDeclsAndExtendTVars(decls, sClasses, tVars)
           
            ProgBlock(
                typecheckedDecls,
                stmts.map(typecheckStmt(_, sClasses, extTVars)),
                typecheckExprWithExpType(expr, expRetType, sClasses, extTVars)
            )
    )
            
    def typecheckDeclsAndExtendTVars(
        decls: List[CleanDecl], sClasses: Map[String, CleanShapeType], tVars: Map[String, CleanType]
    ): (List[DeclWE], Map[String, CleanType]) =

        val (revDeclsWE, extTVars) = 
            decls.foldLeft((List[DeclWE](), tVars)){

                case ((declList, tVarsAcc), curDecl) =>
                    val (declWE, updTVars) = typecheckOneDecl(curDecl, sClasses, tVarsAcc)
                    (declWE :: declList, updTVars)

            }
        
        (revDeclsWE.reverse, extTVars)

    def typecheckOneDecl(
        decl: CleanDecl, sClasses: Map[String, CleanShapeType], tVars: Map[String, CleanType]
    ): (DeclWE, Map[String, CleanType]) = decl match
        case Decl(varDecl, rhs) =>
            inferExprType(rhs, sClasses, tVars) match
                case Right(inferredType) => 
                    (
                        ConverterToWE.declToWE(decl), 
                        tVars.updated(varDecl, inferredType)
                    )

                case Left(exprWE) => 
                    (
                        WE.Node(Decl( WE.Node(varDecl), exprWE )),  
                        tVars
                    )
                        
    
    def typecheckExpr(
        expr: CleanExpr, sClasses: Map[String, CleanShapeType], tVars: Map[String, CleanType]
    ): ExprWE = 
        inferExprType(expr, sClasses, tVars) match
            case Right(inferredType) => 
                ConverterToWE.exprToWE(expr)

            case Left(exprWE) => exprWE

    def typecheckExprWithExpType(
        expr: CleanExpr, expType : CleanType, sClasses: Map[String, CleanShapeType], tVars: Map[String, CleanType]
    ): ExprWE = 
        inferExprType(expr, sClasses, tVars) match
            case Right(inferredType) => 
                if expType == inferredType then
                    ConverterToWE.exprToWE(expr)
                else
                    WE.Err(ExpectedExprTypeMismatch)

            case Left(exprWE) => exprWE

    def inferExprType(
        expr: CleanExpr, sClasses: Map[String, CleanShapeType], tVars: Map[String, CleanType]
    ): Either[ExprWE, CleanType] = expr match
        case Expr.Num(n) => 
            Right(Type.Number())

        case Expr.Var(varRef) => 
            Right(tVars(varRef))

        case Expr.BinOpExpr(lhs, op, rhs) =>
            op match
                case BinOp.Equals => 
                    Right(Type.Number())

                case _ =>
                    (tVars(lhs), tVars(rhs)) match
                        case (Type.Number(), Type.Number()) =>
                            Right(Type.Number())
                        case (_, _) =>
                            Left(WE.Err(BinOpWithNonNumberType))    

        case Expr.NewInstance(cname, args) =>
            sClasses(cname) match
                case Shape(fieldTypes, _) if args.lengthIs != fieldTypes.length => 
                    Left(WE.Err(NewInstanceWrongNumberOfFields))
                    
                case instantiatedShape @ Shape(fieldTypes, _) => 
                    // TODO, this same logic is used in matchingClass function
                    val argsTypematchFields = args.zip(fieldTypes).forall{
                        case (argVarRef, FieldType(_, fieldType)) => 
                            fieldType == tVars(argVarRef)
                    }

                    if argsTypematchFields then
                        Right(instantiatedShape)
                    else 
                        Left(WE.Err(NewInstanceFieldWrongType))

        case Expr.IsInstanceOf(instance, cname) =>
            tVars(instance) match
                case Type.Number() =>
                    Left(WE.Err(IsACalledWithNonNumberType)) 

                case Shape(_, _) =>
                    // sClasses(cname) will always succeed because we know class is in scope
                    Right(Type.Number())

        case Expr.GetField(instance, fname) =>
            tVars(instance) match
                case Type.Number() => 
                    Left(WE.Err(GetFieldCalledOnNonShapeType))

                case s @ Shape(_, _) => 
                    ShapeUtils.getFieldType(s, fname) match
                        case Right(typ) => 
                            Right(typ)
                        case Left(nameWE) => 
                            Left(WE.Node(Expr.GetField(
                                WE.Node(instance),
                                nameWE
                            )))

        case Expr.CallMethod(instance, mname, args) =>
            tVars(instance) match
                case Type.Number() => 
                    Left(WE.Err(CallMethodOnNumberType))
                case s @ Shape(_, _) =>
                    ShapeUtils.getMethodType(s, mname) match
                        case Left(nameWE) => 
                            Left(WE.Node(Expr.CallMethod(
                                WE.Node(instance),
                                nameWE,
                                args.map(WE.Node(_))
                            )))

                        case Right((paramTypes, _)) if args.lengthIs != paramTypes.length => 
                            Left(WE.Err(CallMethodWrongNumberOfParams))

                        case Right((paramTypes, retType)) => 
                            // TODO, this same logic is used in matchingClass function
                            val argsTypematchParams = args.zip(paramTypes).forall{
                                case (argVarRef, paramType) => 
                                    paramType == tVars(argVarRef)
                            }

                            if argsTypematchParams then
                                Right(retType)
                            else 
                                Left(WE.Err(CallMethodParamWrongType))                                   
    
    def typecheckStmt(
        stmt: CleanStmt, sClasses: Map[String, CleanShapeType], tVars: Map[String, CleanType]
    ): StmtWE = WE.Node(stmt match
        case Stmt.Assign(id, rhs) => 
            val expectecExprType = tVars(id)
            Stmt.Assign(
                WE.Node(id), 
                typecheckExprWithExpType(rhs, expectecExprType, sClasses, tVars)
            )

        case Stmt.Ifelse(guard, tbranch, ebranch) =>
            Stmt.Ifelse(
                typecheckExpr(guard, sClasses, tVars), 
                typecheckSBlock(tbranch, sClasses, tVars), 
                typecheckSBlock(ebranch, sClasses, tVars) 
            )                 
            
        case Stmt.While(guard, body) =>
            Stmt.While(
                typecheckExpr(guard, sClasses, tVars), 
                typecheckSBlock(body, sClasses, tVars)
            )

        case Stmt.FieldAssign(instance, fname, rhs) =>
            tVars(instance) match
                case Type.Number() => 
                    Stmt.FieldAssign(
                        WE.Err(InstanceIsNotAShape),
                        WE.Node(fname),
                        typecheckExpr(rhs, sClasses, tVars)
                    )
                case instShape @ Shape(_, _) =>
                    ShapeUtils.getFieldType(instShape, fname) match
                        case Left(nameWE) => 
                            Stmt.FieldAssign(
                                WE.Node(instance),
                                nameWE,
                                typecheckExpr(rhs, sClasses, tVars)
                            )
                        case Right(expectedFieldType) => 
                            Stmt.FieldAssign(
                                WE.Node(instance),
                                WE.Node(fname),
                                typecheckExprWithExpType(rhs, expectedFieldType, sClasses, tVars)
                            )
    )
                                 
    def typecheckSBlock(
        b: CleanStmtBlock, sClasses: Map[String, CleanShapeType], tVars: Map[String, CleanType]
    ) : StmtBlockWE =  WE.Node( b match
        case StmtBlock.One(stmt) => 
            StmtBlock.One(
                typecheckStmt(stmt, sClasses, tVars)
            )

        case StmtBlock.Many(decls, stmts) => 
            val (typecheckedDecls, extTVars) = typecheckDeclsAndExtendTVars(decls, sClasses, tVars)

            StmtBlock.Many(
                typecheckedDecls, 
                stmts.map(typecheckStmt(_, sClasses, extTVars))
            )
    )           
    
object ShapeUtils:

    def getFieldType(s : CleanShapeType, targetFName: String) : Either[NameWE, CleanType] = s match
        case Type.Shape(fieldTypes, methodTypes) =>
            val optTargetFtype = fieldTypes.find(ftype => ftype.fname == targetFName)

            optTargetFtype match
                case Some(FieldType(fname, typ)) => 
                    Right(typ)
                case None =>
                    Left(WE.Err(FieldDoesNotExist))

    def getMethodType(s : CleanShapeType, targetMName: String) : Either[NameWE, (List[CleanType], CleanType)] = s match
        case Type.Shape(fieldTypes, methodTypes) =>
            val optTargetMtype = methodTypes.find(mtype => mtype.mname == targetMName)

            optTargetMtype match
                case Some(MethodType(mname, paramTypes, retType)) =>
                    Right((paramTypes, retType))
                case None =>
                    Left(WE.Err(CallMethodDoesNotExist))