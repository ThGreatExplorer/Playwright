package static

import ast._
import ast.ValidityErrNodes._

object VCheckUndefined:

    // Top Level entry points

    def closedSystem(s: CleanSystem): SystemWE = WE.Node(s match
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

    def closedProg(p: CleanProgram): ProgramWE = WE.Node(p match
        case Program[Clean](clss, progb) =>
            // The scope of a ClassName is the entire program
            val clssInScope = clss.getCNames.toSet
            // Variables obey lexical scope, so we begin with an empty environment
            val varsInScope : Set[String] = Set()

            Program(
                clss.map(closedClass(_, clssInScope)),
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
    def closedModules(mods: List[CleanModule]) : (List[ModuleWE], Map[String, String]) = 

        def closedModulesLoop(
            modsRem: List[CleanModule], modsSoFar: List[ModuleWE], modToCNameMapSoFar : Map[String, String]
        ) : (List[ModuleWE], Map[String, String]) = modsRem match

            case Nil => (modsSoFar.reverse, modToCNameMapSoFar)

            case Module(mname, imports, clas) :: tail => 
                val updModToCNameMap = modToCNameMapSoFar.updated(mname, clas.cname)
                val (validatedImports, clssInScope) = closedImports(imports, modToCNameMapSoFar)
                val processedModule = WE.Node(Module(
                    WE.Node(mname),
                    validatedImports,
                    closedClass(clas, clssInScope)
                ))

                closedModulesLoop(tail, processedModule :: modsSoFar, updModToCNameMap)

        closedModulesLoop(mods, Nil, Map[String, String]())

    /**
     * Validates that every import refers to a ModuleName that is defined in 
     * the preceding sequence of modules. During validation, constructs a set of 
     * classes that would be in scope under this sequence of imports.
     * 
     * @param imports List of imports 
     * @param modToCNameMap Map from ModuleNames to ClassNames that were defined before
     * this sequence of imports
     * @return Tuple of validated Import nodes and set of Classes in scope 
     */ 
    def closedImports(
        imports: List[CleanImportedMod], modToCNameMap : Map[String, String]
    ) : (List[ImportedModWE], Set[String]) =
        val modsInScope      = modToCNameMap.keySet
        val validatedImports = imports.map(closedImportedModName(_, modsInScope))
        val clssInScope      = validatedImports.foldLeft(Map[String, String]()){
                case (acc, WE.Err(_)) => acc
                case (acc, WE.Node(importedMod)) => 
                    acc.updated(importedMod, modToCNameMap(importedMod))
            }.values.toSet

        (validatedImports, clssInScope)

    def closedImportedModName(c: CleanImportedMod, modsInScope: Set[String]): ImportedModWE =
        if modsInScope.contains(c) then
            WE.Node(c)
        else
            WE.Err(ModuleNotDeclared) 

    // Class helpers

    def closedClass(cls : CleanClass, clssInScope: Set[String]) : ClassWE = WE.Node(cls match
        case Class(cname, fields, methods) => 
            Class(
                WE.Node(cname),
                fields.map(WE.Node(_)),
                methods.map(closedMethod(_, clssInScope.incl(cname)))
            )
        )
    
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