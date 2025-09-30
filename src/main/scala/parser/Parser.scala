package parser

import annotation.tailrec

import ast._
import sexprs.SExprs._
import util.ExampleKeyword as Keyword
import util.ExampleKeyword.isKeyword
import ast.Declaration.DeclarationError

object Parser:
    /** Parses the given sexpr into a BareBones program to the best of its ability.
      * If grammar is invalid, an error node is inserted instead.
      *
      * @param sexpr SExpr read in from input
      * @return BareBones AST with possible error nodes if grammar rules are violated
      */
    def parseProg(sexpr: SExpr): Program = sexpr match
        case SList(Nil) => Program.Err(ProgErr.EmptyList)    
        case SList(SList(decls) :: SList(stmts) :: expr :: Nil) =>
            Program.Prog(
                parseDeclsTail(decls),
                parseStmtsTail(stmts),
                parseExpr(expr)
            )
        // TODO how to handle the case where not sure whether list of declarations
        // or statements? It's ambiguous and can be either
        case SList(SList(declsorstmts) :: expr :: Nil) =>
            // for now let's try both and take program valid > decl > stmts
            val progdecl = Program.Prog(
                parseDeclsTail(declsorstmts),
                Nil,
                parseExpr(expr)
            )
            val progstmts = Program.Prog(
                Nil,
                parseStmtsTail(declsorstmts),
                parseExpr(expr)
            )
            if ASTInspector.progHasError(progdecl) && ASTInspector.progHasError(progstmts) then
                progdecl
            else if ASTInspector.progHasError(progdecl) then
                progstmts
            else
                progdecl
        case _ => Program.Err(ProgErr.NotAList)

    
    def parseDeclsTail(decls: List[SExpr]): List[Declaration] =
        @tailrec
        def loop(remaining: List[SExpr], acc: List[Declaration]): List[Declaration] =
            remaining match
                case Nil => acc.reverse
                case h :: t => loop(t, parseDecl(h) :: acc)
        def parseDecl(sexp: SExpr): Declaration = sexp match
            case SList(SSymbol(Keyword.Def) :: lhs :: rhs :: Nil) =>
                Declaration.Declare(
                    parseVar(lhs),
                    parseExpr(rhs)
                )
            case _ => DeclarationError(DeclareError.Malformed)
        loop(decls, Nil)
            
    // Use tail-recursion for parsing lists of statements to avoid stack overflow
    // map() uses recursion for Lists under the hood
    def parseStmtsTail(stmts: List[SExpr]): List[Statement] =
        @tailrec
        def loop(remaining: List[SExpr], acc: List[Statement]): List[Statement] =
            remaining match
                case Nil => acc.reverse
                case h :: t => loop(t, parseStmt(h) :: acc)
        loop(stmts, Nil)

    def parseStmt(sexpr: SExpr): Statement = sexpr match
        // Assignment: (Variable = Expression)
        case SList(name :: SSymbol(Keyword.Assign) :: expr :: Nil) =>
            Statement.Assign(
                parseVar(name), 
                parseExpr(expr)
            )
        case SList(name :: SSymbol(Keyword.Assign) :: _) =>
            Statement.Err(StmtErr.AssignRhsMalformed)
        
        // IfElse: (if0 Expression Block Block)
        case SList(SSymbol(Keyword.If) :: grd :: thn :: els :: Nil) =>
            Statement.Ifelse(
                parseExpr(grd),
                parseBlock(thn),
                parseBlock(els)
            )
        case SList(SSymbol(Keyword.If) :: _) =>
            Statement.Err(StmtErr.IfelseMalformed)

        // While: (while0 Expression Block)
        case SList(SSymbol(Keyword.While) :: grd :: body :: Nil) =>
            Statement.While(
                parseExpr(grd),
                parseBlock(body)
            )
        case SList(SSymbol(Keyword.While) :: _) =>
            Statement.Err(StmtErr.WhileMalformed)

        case _ => Statement.Err(StmtErr.Malformed)

    def parseBlock(sexpr: SExpr): Block = sexpr match
        // Many: (block Statement^+)
        case SList(SSymbol(Keyword.Block) :: Nil) => 
            Block.Err(BlockErr.ManyNoStmts)
        case SList(SSymbol(Keyword.Block) :: SList(decls) :: SList(stmts) :: Nil) =>
            Block.Many(parseDeclsTail(decls), parseStmtsTail(stmts))
        case SList(SSymbol(Keyword.Block) :: SList(stmts) :: Nil) =>
            Block.Many(Nil, parseStmtsTail(stmts))
        // One: Statement
        case _ => 
            Block.One(parseStmt(sexpr))

    def parseExpr(sexpr: SExpr): Expression = sexpr match
        // Num: the set of GoodNumbers comprises all inexact numbers
        //      (doubles) between -1000.0 and +1000.0, inclusive.
        case SDouble(n) => 
            Expression.Num(n)
        // Var: the set of Variables consists of all symboSls, minus keywords
        case SSymbol(s) => 
            parseVar(sexpr)
        // Binops
        // Add:    (Variable + Variable)
        // Div:    (Variable / Variable)
        // Equals: (Variable == Variable)
        case SList(sexp1 :: SSymbol(op) :: sexp2 :: Nil) => {
            val (v1, v2) = (parseVar(sexp1), parseVar(sexp2))
            op match
                case Keyword.Plus =>  Expression.Add(v1, v2)
                case Keyword.Div  =>  Expression.Div(v1, v2)
                case Keyword.Eq   =>  Expression.Equals(v1, v2)
                case _            =>  Expression.Err(ExprErr.BadOperand)
        }
        case _ => 
            Expression.Err(ExprErr.Malformed)

    def parseVar(ssymbol: SExpr): Expression.Var | Expression.Err = ssymbol match
        // Var: the set of Variables consists of all symboSls, minus keywords
        case SSymbol(s) => 
            if !isKeyword(s) then
                Expression.Var(s) 
            else 
                Expression.Err(ExprErr.VarIsKeyword)
        case _ => Expression.Err(ExprErr.VarNotAName)
        

