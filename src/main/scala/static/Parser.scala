package static

import annotation.tailrec

import ast._
import ast.ParseErrNodes._
import sexprs.SExprs._
import util.ExampleKeyword as Keyword
import util.ExampleKeyword.isKeyword

object Parser:

    /** Parses the given sexpr into a BareBones program to the best of its ability.
      * If grammar is invalid, an error node is inserted instead.
      *
      * @param sexpr SExpr read in from input
      * @return BareBones AST with possible error nodes if grammar rules are violated
      */
    def parseProg(sexpr: SExpr): ProgramWE = sexpr match
        // Program: (Declaration^* Statement^* Expression)  
        case SList(Nil) => WE.Err(ProgEmptyList) 
        case SList(elems) => {
            val (classes, decls, stmts) = splitClassDeclsAndStmts(elems.dropRight(1))
            val expr = elems.last
            WE.Node(Program(
                classes.map(parseClass),
                decls.map(parseDecl),
                stmts.map(parseStmt),
                parseExpr(expr)
            ))
        }
        case _ => WE.Err(ProgNotAList)

    /**
      * Splits a list of SExprs into three SExpr lists: 
      *  - First contains longest prefix of Class declarations candidates: zero or more 
      *    SLists with first element being the `class` keyword.
      *  - Second contains longest prefix (after class declarations) of Variable declaration 
      *    candidates: zero or more SLists with first element being the `def` keyword.
      *  - Third contains the rest of the original SExpr list, which we expect to contain
      *    statements.
      * 
      * Since we are using "longest prefix" heuristic, it is posisble to encounter declarations
      * out of place. This will be caught by respective parse functions.
      * 
      * @param elems SExpr list
      * @return Tuple of three SExpr lists: the first contains class declaration candidates,
      *  second contains variable declaration candidates, last contains statement candidates
      */
    def splitClassDeclsAndStmts(elems: List[SExpr]) : (List[SExpr], List[SExpr], List[SExpr]) =
        def loopClass(remaining: List[SExpr], accClass: List[SExpr]) : (List[SExpr], List[SExpr], List[SExpr]) =
            remaining match
                case Nil => (accClass.reverse, Nil, Nil)
                case (head @ SList(SSymbol(Keyword.Class.value) :: _)) :: rest => 
                    loopClass(rest, head :: accClass)
                case rest => 
                    val (decls, stmts) = splitDeclsAndStmts(rest)
                    (accClass.reverse, decls, stmts) 
        loopClass(elems, Nil)
            
    def splitDeclsAndStmts(elems: List[SExpr]) : (List[SExpr], List[SExpr]) = 
        def loopDecl(remaining: List[SExpr], accDecls: List[SExpr]) : (List[SExpr], List[SExpr]) =
            remaining match
                case Nil => (accDecls.reverse, Nil)
                case (head @ SList(SSymbol(Keyword.Def.value) :: _)) :: rest => 
                    loopDecl(rest, head :: accDecls)
                case _ => (accDecls.reverse, remaining)
        loopDecl(elems, Nil)

    def parseClass(sexp: SExpr): ClassWE = sexp match
        // Class: (class ClassName (FieldName^*) Method^*)
        case SList(SSymbol(Keyword.Class.value) :: className :: SList(fieldnames) :: methods) =>
            WE.Node(Class(
                parseName(className),
                fieldnames.map(parseName),
                methods.map(parseMethod)
            ))
        case _ => WE.Err(ClassMalformed)

    def parseMethod(sexp: SExpr): MethodWE = sexp match
        // Method: (method MethodName (Parameter^*) Declaration^* Statement^* Expression)
        case SList(SSymbol(Keyword.Method.value) :: methodName :: SList(params) :: Nil) =>
            WE.Err(MethodNoExpr)
        case SList(SSymbol(Keyword.Method.value) :: methodName :: SList(params) :: rest) =>
            val (decls, stmts) = splitDeclsAndStmts(rest.dropRight(1))
            val expr = rest.last        
            WE.Node(Method(
                parseName(methodName),
                params.map(parseName),
                decls.map(parseDecl),
                stmts.map(parseStmt),
                parseExpr(expr)
            ))
        case _ => WE.Err(MethodMalformed)

    def parseDecl(sexp: SExpr): DeclWE = sexp match
        // Declaration: (def Variable Expression)
        case SList(SSymbol(Keyword.Def.value) :: lhs :: rhs :: Nil) =>
            WE.Node(Decl(
                parseName(lhs),
                parseExpr(rhs)
            ))
        case _ => WE.Err(DeclMalformed)

    def parseStmt(sexpr: SExpr): StmtWE = sexpr match
        case SList(SSymbol(Keyword.Def.value) :: _) =>
            WE.Err(DeclAtStmtPosition)

        // Assignment: (Variable = Expression)
        case SList(name :: SSymbol(Keyword.Assign.value) :: expr :: Nil) =>
            WE.Node(Stmt.Assign(
                parseVarRef(name), 
                parseExpr(expr)
            ))
        case SList(name :: SSymbol(Keyword.Assign.value) :: _) =>
            WE.Err(AssignRhsMalformed)
        
        // IfElse: (if0 Expression Block Block)
        case SList(SSymbol(Keyword.If.value) :: grd :: thn :: els :: Nil) =>
            WE.Node(Stmt.Ifelse(
                parseExpr(grd),
                parseBlock(thn),
                parseBlock(els)
            ))
        case SList(SSymbol(Keyword.If.value) :: _) =>
            WE.Err(IfelseMalformed)

        // While: (while0 Expression Block)
        case SList(SSymbol(Keyword.While.value) :: grd :: body :: Nil) =>
            WE.Node(Stmt.While(
                parseExpr(grd),
                parseBlock(body)
            ))
        case SList(SSymbol(Keyword.While.value) :: _) =>
            WE.Err(WhileMalformed)

        // Field Assignment: (Variable --> FieldName = Expression)
        case SList(variable :: SSymbol(Keyword.Accessor.value) :: fieldName :: SSymbol(Keyword.Assign.value) :: expr :: Nil) =>
            WE.Node(Stmt.FieldAssign(
                parseVarRef(variable),
                parseName(fieldName),
                parseExpr(expr)
            ))

        case _ => WE.Err(StmtMalformed)

    def parseBlock(sexpr: SExpr): BlockWE = sexpr match
        // Many: (block Declaration^* Statement^+)
        case SList(SSymbol(Keyword.Block.value) :: Nil) => 
            WE.Err(BlockManyNoStmts)
        case SList(SSymbol(Keyword.Block.value) :: elems) => {
            // Ensures that we have at least one statement
            splitDeclsAndStmts(elems) match
                case (decls, Nil) => 
                    WE.Err(BlockManyNoStmts)
                case (decls, stmts) => 
                    WE.Node(Block.Many(
                        decls.map(parseDecl),
                        stmts.map(parseStmt)
                    ))
        }
            
        // One: Statement
        case _ => 
            WE.Node(Block.One(parseStmt(sexpr)))

    def parseExpr(sexpr: SExpr): ExprWE = sexpr match
        // Num: the set of GoodNumbers comprises all inexact numbers
        //      (doubles) between -1000.0 and +1000.0, inclusive.
        case SDouble(n) => 
            WE.Node(Expr.Num(n))

        // Var: the set of Variables consists of all symboSls, minus keywords
        case SSymbol(s) => 
            WE.Node(Expr.Var(parseVarRef(sexpr)))

        //  (new ClassName (Variable^*))
        case SList(SSymbol(Keyword.New.value) :: className :: SList(variables) :: Nil) =>
            WE.Node(Expr.NewInstance(
                parseName(className),
                variables.map(parseVarRef)
            ))
        
        //  (Variable --> FieldName)
        case SList(variable :: SSymbol(Keyword.Accessor.value) :: fieldName :: Nil) =>
            WE.Node(Expr.GetField(parseVarRef(variable), parseName(fieldName)))

        //  (Variable --> MethodName (Variable^*))
        case SList(variable :: SSymbol(Keyword.Accessor.value) :: methodName :: SList(variables) :: Nil) =>
            WE.Node(Expr.CallMethod(
                parseVarRef(variable),
                parseName(methodName),
                variables.map(parseVarRef)
            ))

        //  (Variable isa ClassName)
        case SList(variable :: SSymbol(Keyword.IsA.value) :: className :: Nil) =>
            WE.Node(Expr.IsInstanceOf(
                parseVarRef(variable),
                parseName(className)
            ))

        // Binops
        // Add:    (Variable + Variable)
        // Div:    (Variable / Variable)
        // Equals: (Variable == Variable)
        case SList(sexp1 :: SSymbol(op) :: sexp2 :: Nil) => {
            val (v1 : VarRefWE, v2 : VarRefWE) = (parseVarRef(sexp1), parseVarRef(sexp2))
            op match
                case Keyword.Plus.value     =>  WE.Node(Expr.BinOpExpr(v1, BinOp.Add, v2))
                case Keyword.Div.value      =>  WE.Node(Expr.BinOpExpr(v1, BinOp.Div, v2))
                case Keyword.Eq.value       =>  WE.Node(Expr.BinOpExpr(v1, BinOp.Equals, v2))
                case _                      =>  WE.Err(ExprBadOperand)
        }
        
        case _ => 
            WE.Err(ExprMalformed)

    def parseVarRef(ssymbol : SExpr) : VarRefWE = 
        ssymbol match
        case SSymbol(s) =>
            if !isKeyword(s) then WE.Node(VarRef(s))
            else WE.Err(NameIsKeyword) 
        case _ =>
            WE.Err(NotAName)

    def parseName(ssymbol : SExpr) : NameWE = 
        ssymbol match
        case SSymbol(s) =>
            if !isKeyword(s) then WE.Node(Name(s))
            else WE.Err(NameIsKeyword) 
        case _ =>
            WE.Err(NotAName)

