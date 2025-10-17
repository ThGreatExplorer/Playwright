package static

import ast._
import ast.ValidityErrNodes._

object VCheckUndefined:

    def closedProg(p: CleanProgram): ProgramWE = WE.Node(p match
        case CleanProgram(clss, decls, stmts, expr) =>
            // The scope of a ClassName is the entire program
            val clssInScope = initClassEnv(clss)
            // Variables obey lexical scope, so we begin with an empty environment
            val initVarsInScope : Set[String] = Set()
            val (validatedDecls, varsInScope) = closedDecls(decls, clssInScope, initVarsInScope)

            Program(
                clss.map(closedClass(_, clssInScope)),
                validatedDecls,
                stmts.map(closedStmt(_,  clssInScope, varsInScope)),
                closedExpr(expr, clssInScope, varsInScope)
            )
        )
    
    def initClassEnv(clss: List[CleanClass]) : Set[String] = 
        clss.foldLeft(Set())((acc, curClass) => acc.incl(curClass.cname.x))

    def closedClass(cls : CleanClass, clssInScope: Set[String]) : ClassWE = WE.Node(cls match
        case Class(cname, fields, methods) => 
            Class(
                ConverterToWE.nameToWE(cname),
                fields.map(ConverterToWE.nameToWE),
                methods.map(closedMethod(_, clssInScope))
            )
        )
    
    def closedMethod(mth : CleanMethod, clssInScope: Set[String]) : MethodWE = WE.Node( mth match
        case Method(mname, params, decls, stmts, expr) => 
            // Parameters are implicitly declared before the method body
            val initVarsInScope : Set[String] = params.foldLeft(Set("this"))((acc, pname) => acc.incl(pname.x))
            val (validatedDecls, varsInScope) = closedDecls(decls, clssInScope, initVarsInScope)

            Method(
                ConverterToWE.nameToWE(mname),
                params.map(ConverterToWE.nameToWE),
                validatedDecls,
                stmts.map(closedStmt(_, clssInScope, varsInScope)),
                closedExpr(expr, clssInScope, varsInScope)
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
        def processDecl(varDeclName : CleanName, rhs : CleanExpr, varsInScope : Set[String]) : DeclWE =
            WE.Node(Decl(
                ConverterToWE.nameToWE(varDeclName),
                closedExpr(rhs, clssInScope, varsInScope)
            ))

        def closedDeclsLoop(declsRem: List[CleanDecl], declsSoFar: List[DeclWE], dvarsSoFar: Set[String]) 
        : (List[DeclWE], Set[String]) = declsRem match
            case Nil => (declsSoFar.reverse, dvarsSoFar)
            case Decl(varDeclName, rhs) :: tail => 
                val processedDecl = processDecl(varDeclName, rhs, dvarsSoFar)
                closedDeclsLoop(tail, processedDecl :: declsSoFar, dvarsSoFar.incl(varDeclName.x))

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
                closedBlock(tbranch, clssInScope, varsInScope), 
                closedBlock(ebranch, clssInScope, varsInScope) 
            )

        case Stmt.While(guard, body) =>
            Stmt.While(
                closedExpr(guard, clssInScope, varsInScope),
                closedBlock(body, clssInScope, varsInScope)
            )

        case Stmt.FieldAssign(instance, fname, rhs) =>
            Stmt.FieldAssign(
                closedVarRef(instance, varsInScope), 
                ConverterToWE.nameToWE(fname),
                closedExpr(rhs, clssInScope, varsInScope)
            )
        )
        
        
    def closedBlock(block: CleanBlock, clssInScope : Set[String], varsInScope: Set[String]): BlockWE = WE.Node( block match
        case Block.One(stmt) => 
            Block.One(closedStmt(stmt, clssInScope, varsInScope))

        case Block.Many(decls, stmts) => 
            val (processedDecls, extVarsInScope) = closedDecls(decls, clssInScope, varsInScope)
            Block.Many(
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
                ConverterToWE.nameToWE(fname)
            )
        
        case Expr.CallMethod(instance, mname, args) =>
            Expr.CallMethod(
                closedVarRef(instance, varsInScope),
                ConverterToWE.nameToWE(mname),
                args.map(closedVarRef(_, varsInScope))
            )
        
    )

    def closedVarRef(v: CleanVarRef, varsInScope: Set[String]): VarRefWE  = v match
        case VarRef(x) if varsInScope.contains(x) =>
            WE.Node(VarRef(x))
        case _ =>
            WE.Err(VarNotDeclared) 
    
    def closedClassNameRef(c: CleanName, clssInScope: Set[String]): NameWE = c match
        case Name(x) if clssInScope.contains(x) =>
            WE.Node(Name(x))
        case _ =>
            WE.Err(ClassNotDeclared)