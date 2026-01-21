package static

import ast._
import ast.ValidityErrNodes._
import ast.ConverterToWE.shapeToWE
import util.{getCNames}
import ast.ConverterToWE.stringToWE
import ast.ConverterToWE.optionalShapeToWE

object VCheckUndefined:

    // Top Level entry points

    def closedSystem(s: CleanSystem): SystemWE = WE.Node(s match
        case System[Clean](modules, imports, progb, moduleData, range) =>

            val moduleData = ModuleData(modules)

            val validatedModules = closedModules(modules, moduleData)
            // Classes in scope constructed based on the sequence of modules 
            val (validatedImports, clssInScope)   = closedImports(imports, moduleData.atTopLevel)
            // Variables obey lexical scope, so we begin with an empty environment
            val varsInScope = Set[String]()

            System(
                validatedModules,
                validatedImports,
                closedProgBlock(progb, clssInScope, varsInScope), 
                moduleData,
                range
            )
        )

    def closedProg(p: CleanProgram): ProgramWE = WE.Node(p match
        case Program[Clean](clss, progb, range) =>
            // The scope of a ClassName is the entire program
            val clssInScope = clss.getCNames.toSet
            // Variables obey lexical scope, so we begin with an empty environment
            val varsInScope : Set[String] = Set()

            Program(
                clss.map(closedClass(_, clssInScope)),
                closedProgBlock(progb, clssInScope, varsInScope),
                range
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
    def closedModules(mods: List[CleanModule], moduleData : ModuleData) : List[ModuleWE] = 

        def checkOneMod(m : CleanModule) : ModuleWE = m match
            case Module(mname, imports, clas, range) => 
                val (validatedImports, clssInScope) = closedImports(imports, moduleData.scopedAt(mname))
                WE.Node(Module(
                    WE.Node(mname),
                    validatedImports,
                    closedClass(clas, clssInScope),
                    range
                ))

        def closedModulesLoop(
            modsRem: List[CleanModule], modsSoFar: List[ModuleWE]
        ) : List[ModuleWE] = modsRem match

            case Nil => modsSoFar.reverse

            case (module: CleanModule) :: tail => 
                val processedModule = checkOneMod(module)
                closedModulesLoop(tail, processedModule :: modsSoFar)

        closedModulesLoop(mods, Nil)

    /**
     * Validates that every import refers to a ModuleName that is defined in 
     * the preceding sequence of modules. During validation, constructs a set of 
     * classes that would be in scope under this sequence of imports.
     * 
     * @param imports List of imports 
     * @param moduleData Module Data map for quick lookups scoped at the current module
     * @return Tuple of validated Import nodes and set of Classes in scope 
     */ 
    def closedImports(
        imports: List[CleanImport], moduleData : ScopedModuleData
    ) : (List[ImportWE], Set[String]) =

        def closedImportName(c: CleanImport): ImportWE = c match
            case (i : CleanImport) if !moduleData.contains(i.importedModName) => 
                WE.Err(ModuleNotDeclared)

            case Import.Untyped(mname, range) =>
                WE.Node(Import.Untyped(stringToWE(mname), range))

            case Import.Typed(mname, shape, range) =>
                WE.Node(Import.Typed(
                    stringToWE(mname), 
                    shapeToWE(shape),
                    range
                ))

        val validatedImports = imports.map(closedImportName(_))
        val clssInScope   =
            validatedImports
            .flatMap{
                case WE.Node(Import.Untyped(WE.Node(mname), _))  => Some(mname)
                case WE.Node(Import.Typed(WE.Node(mname), _, _)) => Some(mname)
                case WE.Err(e) => None
                case _         => None
            }
            .map(moduleName => moduleData.lookupModuleCName(moduleName))
            .toSet

        (validatedImports, clssInScope)

    // Class helpers

    def closedClass(cls : CleanClass, clssInScope: Set[String]) : ClassWE = WE.Node(cls match
        case Class(cname, fields, methods, shape, range) => 
            Class(
                WE.Node(cname),
                fields.map(WE.Node(_)),
                methods.map(closedMethod(_, clssInScope.incl(cname))),
                optionalShapeToWE(shape),
                range
            )
        )
    
    def closedMethod(mth : CleanMethod, clssInScope: Set[String]) : MethodWE = WE.Node( mth match
        case Method(mname, params, pblock, range) => 
            // Parameters are implicitly declared before the method body with "this"
            val varsInScope : Set[String] = ("this" :: params).toSet

            Method(
                WE.Node(mname),
                params.map(WE.Node(_)),
                closedProgBlock(pblock, clssInScope, varsInScope),
                range
            )
        )

    // Core helpers

    def closedProgBlock(pb: CleanProgBlock, clssInScope: Set[String], varsInScope: Set[String]) : ProgBlockWE = WE.Node( pb match
        case ProgBlock(decls, stmts, expr, range) => 
            val (validatedDecls, extVarsInScope) = closedDecls(decls, clssInScope, varsInScope)

            ProgBlock(
                validatedDecls,
                stmts.map(closedStmt(_,  clssInScope, extVarsInScope)),
                closedExpr(expr, clssInScope, extVarsInScope),
                range
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

            case Decl(name, rhs, range) :: tail => 
                val processedDecl = WE.Node(Decl(
                    WE.Node(name),
                    closedExpr(rhs, clssInScope, dvarsSoFar),
                    range
                ))

                closedDeclsLoop(tail, processedDecl :: declsSoFar, dvarsSoFar.incl(name))

        closedDeclsLoop(decls, Nil, varsInScope)

    def closedStmt(stmt: CleanStmt, clssInScope : Set[String], varsInScope: Set[String]): StmtWE = WE.Node( stmt match
        case Stmt.Assign(id, rhs, range) => 
            Stmt.Assign(
                closedVarRef(id, varsInScope), 
                closedExpr(rhs, clssInScope, varsInScope),
                range
            )

        case Stmt.Ifelse(guard, tbranch, ebranch, range) =>
            Stmt.Ifelse(
                closedExpr(guard, clssInScope, varsInScope), 
                closedSBlock(tbranch, clssInScope, varsInScope), 
                closedSBlock(ebranch, clssInScope, varsInScope),
                range
            )

        case Stmt.While(guard, body, range) =>
            Stmt.While(
                closedExpr(guard, clssInScope, varsInScope),
                closedSBlock(body, clssInScope, varsInScope),
                range
            )

        case Stmt.FieldAssign(instance, fname, rhs, range) =>
            Stmt.FieldAssign(
                closedVarRef(instance, varsInScope), 
                WE.Node(fname),
                closedExpr(rhs, clssInScope, varsInScope),
                range
            )
        )
        
    def closedSBlock(b: CleanStmtBlock, clssInScope : Set[String], varsInScope: Set[String]): StmtBlockWE = WE.Node( b match
        case StmtBlock.One(stmt, range) => 
            StmtBlock.One(closedStmt(stmt, clssInScope, varsInScope), range)

        case StmtBlock.Many(decls, stmts, range) => 
            val (processedDecls, extVarsInScope) = closedDecls(decls, clssInScope, varsInScope)
            StmtBlock.Many(
                processedDecls, 
                stmts.map(closedStmt(_, clssInScope, extVarsInScope)),
                range
            )
        )
            
    def closedExpr(e: CleanExpr, clssInScope : Set[String], varsInScope: Set[String]): ExprWE = WE.Node(e match
        case Expr.Num(n, range) => Expr.Num(n, range)

        case Expr.Var(x, range) => 
            Expr.Var(closedVarRef(x, varsInScope), range)

        case Expr.BinOpExpr(lhs, op, rhs, range) => 
            Expr.BinOpExpr(
                closedVarRef(lhs, varsInScope), 
                op,
                closedVarRef(rhs, varsInScope),
                range
            )
        
        case Expr.NewInstance(cname, args, range) =>
            Expr.NewInstance(
                closedClassNameRef(cname, clssInScope),
                args.map(closedVarRef(_, varsInScope)),
                range
            )

        case Expr.IsInstanceOf(instance, cname, range) =>
            Expr.IsInstanceOf(
                closedVarRef(instance, varsInScope),
                closedClassNameRef(cname, clssInScope),
                range
            )

        case Expr.GetField(instance, fname, range) =>
            Expr.GetField(
                closedVarRef(instance, varsInScope),
                WE.Node(fname),
                range
            )
        
        case Expr.CallMethod(instance, mname, args, range) =>
            Expr.CallMethod(
                closedVarRef(instance, varsInScope),
                WE.Node(mname),
                args.map(closedVarRef(_, varsInScope)),
                range
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
