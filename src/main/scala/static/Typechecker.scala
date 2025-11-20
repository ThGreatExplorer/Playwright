package static

import ast._
import ast.TypeErrorNodes._
import ast.Type.Shape
import util.{getCNames, getFTypeNames, getMTypeNames}
import util.UnreachablePatternMatch

// object Typechecker:

object Typechecker:

    enum Inferred:
        case Top
        case Concrete(t : CleanType)

        def isConsistentWith(that : CleanType) : Boolean =
            this match
                case Top => true
                case Concrete(t) => t == that

    object Inferred:
        def apply(t : CleanType) : Inferred.Concrete = Inferred.Concrete(t)
            
    type TVarsMap    = Map[String, Inferred]
    type SClassesMap = Map[String, CleanShapeType]

    // Top Level entry point

    def typecheckSystem(s: CleanSystem): SystemWE = WE.Node( s match
        case System[Clean](modules, imports, progb, moduleData) =>

            val typecheckedModules = typecheckModules(modules, moduleData)
            // Classes in scope constructed based on the sequence of modules 
            val (typecheckedImports, sClasses) = typecheckImports(imports, moduleData.atTopLevel)
            // Variables obey lexical scope, so we begin with an empty Map from variables to Types
            val tVars : TVarsMap = Map.empty

            val topLevelExpectedReturnType : CleanType = Type.Number()

            System(
                typecheckedModules,
                typecheckedImports,
                typecheckProgBWithExpType(progb, topLevelExpectedReturnType, sClasses, tVars), 
                moduleData
            )
    )

    // Module helpers

    // (ClassName, Option[Shape])
    type MixedModSignature = (String, Option[CleanShapeType])
    type MixedModToSignatureMap = Map[String, MixedModSignature]
    // (ClassName, Shape)
    type ModSignature = (String, CleanShapeType)
    type ModToSignatureMap = Map[String, ModSignature]

    /**
    * Validates that each module is closed with respect to collection of imported classes.
    *
    * @param mods List of CleanModule to be processed
    * @param moduleData Module Data map for quick lookups
    * @return Tuple of validated Module nodes 
    */
    def typecheckModules(mods: List[CleanModule], moduleData : ModuleData) : List[ModuleWE] = 

        def typecheckOneModule(module: CleanModule) : ModuleWE = module match
            case m @ Module.Untyped(mname, imports, clas) => 
                ConverterToWE.moduleToWE(m) 

            case m @ Module.Typed(mname, imports, clas, shape) =>
                val (typecheckedImports, sClasses) = typecheckImports(imports, moduleData.scopedAt(mname))

                val updatedSClasses = sClasses.updated(clas.cname, shape)
         
                WE.Node(Module.Typed(
                    WE.Node(mname),
                    typecheckedImports,
                    typecheckClass(clas, updatedSClasses),
                    ConverterToWE.shapeToWE(shape) 
                ))

        def typecheckModulesLoop(
            modsRem: List[CleanModule], modsSoFar: List[ModuleWE]
        ) : List[ModuleWE] = modsRem match

            case Nil => modsSoFar.reverse

            case (module: CleanModule) :: tail => 
                val processedModule = typecheckOneModule(module)
                typecheckModulesLoop(tail, processedModule :: modsSoFar)

        typecheckModulesLoop(mods, Nil)

    /**
     * Constructs the sClasses Map from class name to Shape of all the classes that are in scope for
     * the scope of this sequence of imports.
     * 
     * @param imports List of imports 
     * @param moduleData Module Data map for quick lookups scoped at the current module
     * @return Tuple of ImportWE and Map from Class Names to their Shapes 
     */ 
    def typecheckImports(
        imports: List[CleanImport], moduleData : ScopedModuleData
    ) : (List[ImportWE], SClassesMap)  =

        def collectSClassesMapLoop(
            impsRem: List[CleanImport], importedClasses : SClassesMap
        ) : SClassesMap = impsRem match

            case Nil => importedClasses

            case (imp @ Import.Untyped(mname)) :: tail =>
                val (cname, shape) = moduleData.lookupTypedCNameAndShape(mname) 
                val updImportedSoFar = importedClasses.updated(cname, shape)
                collectSClassesMapLoop(tail, updImportedSoFar)

            case (imp @ Import.Typed(mname, shape)) :: tail => 
                val cname = moduleData.lookupUntypedCName(mname) 
                val updImportedSoFar = importedClasses.updated(cname, shape)
                collectSClassesMapLoop(tail, updImportedSoFar)

        val importsWE = imports.map(ConverterToWE.importToWE(_))
        val sClasses  = collectSClassesMapLoop(imports, Map.empty)
        (importsWE, sClasses)

   
    // Class helpers

    def typecheckClass(cls : CleanClass, sClasses: SClassesMap) : ClassWE = 
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
        m: CleanMethod, expectedMtype: CleanMethodType, sClasses: SClassesMap, tVarThis : CleanType
    ): MethodWE = (m, expectedMtype) match
        case (Method(mname, _, _), MethodType(mtname, _, _)) if mname != mtname =>
            WE.Err(ShapeTypeMethodNameMismatch)

        case (Method(_, params, _), MethodType(_, paramTypes, _)) if params.lengthIs != paramTypes.length =>
            WE.Err(ShapeTypeMethodWrongNumberOfParams)

        case (Method(mname, params, progb), MethodType(_, paramTypes, retType)) =>
            val paramITypes = paramTypes.map(Inferred(_))
            val paramTVars: TVarsMap = params.zip(paramITypes).toMap
            val initTVars = Map("this" -> Inferred(tVarThis)) ++ paramTVars

            WE.Node(Method(
                WE.Node(mname),
                params.map(WE.Node(_)),
                typecheckProgBWithExpType(progb, retType, sClasses, initTVars)
            ))
                            
    // Core helpers

    def typecheckProgBWithExpType(
        progb: CleanProgBlock, expRetType : CleanType, sClasses: SClassesMap, tVars: TVarsMap
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
        decls: List[CleanDecl], sClasses: SClassesMap, tVars: TVarsMap
    ): (List[DeclWE], TVarsMap) =

        val (revDeclsWE, extTVars) = 
            decls.foldLeft((List[DeclWE](), tVars)){

                case ((declList, tVarsAcc), curDecl) =>
                    val (declWE, updTVars) = typecheckOneDecl(curDecl, sClasses, tVarsAcc)
                    (declWE :: declList, updTVars)

            }
        
        (revDeclsWE.reverse, extTVars)

    def typecheckOneDecl(
        decl: CleanDecl, sClasses: SClassesMap, tVars: TVarsMap
    ): (DeclWE, TVarsMap) = decl match
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
                        tVars.updated(varDecl, Inferred.Top)
                    )
                        
    def typecheckStmt(
        stmt: CleanStmt, sClasses: SClassesMap, tVars: TVarsMap
    ): StmtWE = WE.Node(stmt match
        case assign @ Stmt.Assign(id, rhs) => 
            typecheckStmtAssign(assign, sClasses, tVars)

        case ifelse @ Stmt.Ifelse(guard, tbranch, ebranch) =>
            typecheckStmtIfelse(ifelse, sClasses, tVars)
            
        case whileStmt @ Stmt.While(guard, body) =>
            typecheckStmtWhile(whileStmt, sClasses, tVars)

        case fieldAssign @ Stmt.FieldAssign(varRef, fname, rhs) =>
            typecheckStmtFieldAssign(fieldAssign, sClasses, tVars)
    )

    def typecheckStmtAssign(assign: Clean[Stmt.Assign[Clean]], sClasses: SClassesMap, tVars: TVarsMap): Stmt.Assign[WE] = 
        assign match
            case Stmt.Assign(id, rhs) => 
                tVars(id) match
                    case Inferred.Concrete(expectedExprType) => 
                        Stmt.Assign(
                            WE.Node(id), 
                            typecheckExprWithExpType(rhs, expectedExprType, sClasses, tVars)
                        )
                    case Inferred.Top => 
                        Stmt.Assign(
                            WE.Node(id), 
                            typecheckExpr(rhs, sClasses, tVars)
                        )

    def typecheckStmtIfelse(ifelse: Clean[Stmt.Ifelse[Clean]], sClasses: SClassesMap, tVars: TVarsMap): Stmt.Ifelse[WE] = 
        ifelse match
            case Stmt.Ifelse(guard, tbranch, ebranch) =>
                Stmt.Ifelse(
                    typecheckExpr(guard, sClasses, tVars), 
                    typecheckSBlock(tbranch, sClasses, tVars), 
                    typecheckSBlock(ebranch, sClasses, tVars) 
                )

    def typecheckStmtWhile(whileStmt: Clean[Stmt.While[Clean]], sClasses: SClassesMap, tVars: TVarsMap): Stmt.While[WE] = 
        whileStmt match
            case Stmt.While(guard, body) =>
                Stmt.While(
                    typecheckExpr(guard, sClasses, tVars), 
                    typecheckSBlock(body, sClasses, tVars)
                )

    def typecheckStmtFieldAssign(fieldAssign: Clean[Stmt.FieldAssign[Clean]], sClasses: SClassesMap, tVars: TVarsMap): Stmt.FieldAssign[WE] = 
        fieldAssign match
            case Stmt.FieldAssign(varRef, fname, rhs) =>
                tVars(varRef) match
                    case Inferred.Concrete(s @ Shape(_, _)) =>
                        ShapeUtils.getFieldType(s, fname) match
                            case Left(nameWE) => 
                                Stmt.FieldAssign(
                                    WE.Node(varRef),
                                    nameWE,
                                    typecheckExpr(rhs, sClasses, tVars)
                                )
                            case Right(expectedFieldType) => 
                                Stmt.FieldAssign(
                                    WE.Node(varRef),
                                    WE.Node(fname),
                                    typecheckExprWithExpType(rhs, expectedFieldType, sClasses, tVars)
                                )
                    
                    case Inferred.Top => 
                        Stmt.FieldAssign(
                            WE.Node(varRef),
                            WE.Node(fname),
                            typecheckExpr(rhs, sClasses, tVars)
                        )

                    case _ =>
                        Stmt.FieldAssign(
                            WE.Err(ExpectedShapeType),
                            WE.Node(fname),
                            typecheckExpr(rhs, sClasses, tVars)
                        )
                                 
    def typecheckSBlock(
        b: CleanStmtBlock, sClasses: SClassesMap, tVars: TVarsMap
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
    

    def typecheckExpr(
        expr: CleanExpr, sClasses: SClassesMap, tVars: TVarsMap
    ): ExprWE = 
        inferExprType(expr, sClasses, tVars) match
            case Right(inferredType) => 
                ConverterToWE.exprToWE(expr)

            case Left(exprWE) => exprWE

    def typecheckExprWithExpType(
        expr: CleanExpr, expType : CleanType, sClasses: SClassesMap, tVars: TVarsMap
    ): ExprWE = 
        inferExprType(expr, sClasses, tVars) match
            case Right(inferredType) => 
                if inferredType.isConsistentWith(expType) then
                    ConverterToWE.exprToWE(expr)
                else
                    WE.Err(ExpectedExprTypeMismatch)

            case Left(exprWE) => exprWE

    def inferExprType(
        expr: CleanExpr, sClasses: SClassesMap, tVars: TVarsMap
    ): Either[ExprWE, Inferred] = expr match
        case Expr.Num(n) => Right(Inferred(Type.Number()))

        case Expr.Var(varRef) => Right(tVars(varRef))                    

        case binOp @ Expr.BinOpExpr(lhs, op, rhs) =>
            inferExprTypeBinOpExpr(binOp, tVars)

        case newInst @ Expr.NewInstance(cname, args) =>
            inferExprTypeNewInst(newInst, sClasses, tVars)

        case isInstance @ Expr.IsInstanceOf(varRef, cname) =>      
            inferExprTypeIsInstanceOf(isInstance, tVars)

        case getField @ Expr.GetField(varRef, fname) =>
            inferExprTypeGetField(getField, tVars)

        case callMethod @ Expr.CallMethod(varRef, mname, args) =>
            inferExprTypeCallMethod(callMethod, tVars)
    
    def inferExprTypeBinOpExpr(binOp: Clean[Expr.BinOpExpr[Clean]], tVars: TVarsMap): Either[ExprWE, Inferred] = 
        binOp match
            case Expr.BinOpExpr(lhs, op, rhs) => 
                (tVars(lhs), op, tVars(rhs)) match
                        case (_, BinOp.Equals, _) => 
                            Right(Inferred(Type.Number()))

                        case (Inferred.Concrete(Type.Number()), _, Inferred.Concrete(Type.Number())) => 
                            Right(Inferred(Type.Number()))

                        case (Inferred.Top, _, _) => 
                            Right(Inferred.Top)
                        
                        case (_, _, Inferred.Top) => 
                            Right(Inferred.Top)

                        case _ => 
                            Left(WE.Err(BinOpWithNonNumberType))

    def inferExprTypeNewInst(newInst: Clean[Expr.NewInstance[Clean]], sClasses: SClassesMap, tVars: TVarsMap): Either[ExprWE, Inferred] = 
        newInst match
            case Expr.NewInstance(cname, args) => 
                val expectedShape = sClasses(cname)

                expectedShape match
                    case Shape(fieldTypes, _) if args.lengthIs != fieldTypes.length => 
                        Left(WE.Err(NewInstanceWrongNumberOfFields))
                        
                    case Shape(fieldTypes, _) => 
                        // TODO, this same logic is used in matchingClass function
                        val argsTypematchFields = args.zip(fieldTypes).forall{
                            case (argVarRef, FieldType(_, fieldType)) => 
                                tVars(argVarRef).isConsistentWith(fieldType)
                        }

                        if argsTypematchFields then
                            Right(Inferred(expectedShape))
                        else 
                            Left(WE.Err(NewInstanceFieldWrongType))

    def inferExprTypeIsInstanceOf(isInstance: Clean[Expr.IsInstanceOf[Clean]], tVars: TVarsMap): Either[ExprWE, Inferred] = 
    isInstance match
        case Expr.IsInstanceOf(varRef, cname) =>
            // sClasses(cname) will always succeed because we know class is in scope                 
            tVars(varRef) match
                case Inferred.Concrete(Shape(_, _)) => Right(Inferred(Type.Number()))
        
                case Inferred.Top => Right(Inferred.Top)

                case _ => Left(
                    WE.Node(Expr.IsInstanceOf(
                        WE.Err(ExpectedShapeType), 
                        WE.Node(cname))
                    ))

    def inferExprTypeGetField(getField: Clean[Expr.GetField[Clean]], tVars: TVarsMap): Either[ExprWE, Inferred] = 
        getField match
            case Expr.GetField(varRef, fname) =>
                tVars(varRef) match
                    case Inferred.Concrete(s @ Shape(_, _)) => 
                        ShapeUtils.getFieldType(s, fname) match
                            case Right(typ) => 
                                Right(Inferred(typ))
                            case Left(nameWE) => 
                                Left(WE.Node(Expr.GetField(WE.Node(varRef), nameWE))) 

                    case Inferred.Top => Right(Inferred.Top)

                    case _ => Left(
                        WE.Node(Expr.GetField(
                            WE.Err(ExpectedShapeType), 
                            WE.Node(fname))
                        ))

    def inferExprTypeCallMethod(callMethod: Clean[Expr.CallMethod[Clean]], tVars: TVarsMap): Either[ExprWE, Inferred] = 
        callMethod match
            case Expr.CallMethod(varRef, mname, args) =>
                tVars(varRef) match

                    case Inferred.Concrete(s @ Shape(_, _)) =>
                        ShapeUtils.getMethodType(s, mname) match

                            case Left(nameWE) => 
                                Left(WE.Node(Expr.CallMethod(
                                    WE.Node(varRef),
                                    nameWE,
                                    args.map(WE.Node(_))
                                )))

                            case Right((paramTypes, _)) if args.lengthIs != paramTypes.length => 
                                Left(WE.Err(CallMethodWrongNumberOfParams))

                            case Right((paramTypes, retType)) => 
                                // TODO, this same logic is used in matchingClass function
                                val argsTypematchParams = args.zip(paramTypes).forall{
                                    case (argVarRef, paramType) => 
                                        tVars(argVarRef).isConsistentWith(paramType)
                                }

                                if argsTypematchParams then
                                    Right(Inferred(retType))
                                else 
                                    Left(WE.Err(CallMethodParamWrongType))    

                    case Inferred.Top => Right(Inferred.Top)

                    case _ => Left(
                        WE.Node(Expr.CallMethod(
                            WE.Err(ExpectedShapeType), 
                            WE.Node(mname),
                            args.map(WE.Node(_))
                        )))

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