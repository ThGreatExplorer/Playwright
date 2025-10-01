package frontend

import annotation.tailrec

import ast._
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
    def parseProg(sexpr: SExpr): Program = sexpr match
        // Program: (Declaration^* Statement^* Expression)  
        case SList(Nil) => Program.Err(ProgErr.EmptyList) 
        case SList(elems) => {
            val (decls, stmts) = splitDeclsAndStmts(elems.dropRight(1))
            val expr = elems.last
            Program.Prog(
                decls.map(parseDecl),
                stmts.map(parseStmt),
                parseExpr(expr)
            )
        }
        case _ => Program.Err(ProgErr.NotAList)

    def splitDeclsAndStmts(elems: List[SExpr]) : (List[SExpr], List[SExpr]) = 
        def loopDecl(remaining: List[SExpr], accDecls: List[SExpr]) : (List[SExpr], List[SExpr]) =
            remaining match
                case Nil => (accDecls.reverse, Nil)
                case (head @ SList(SSymbol(Keyword.Def) :: _)) :: rest => 
                    loopDecl(rest, head :: accDecls)
                case _ => (accDecls.reverse, remaining)
        loopDecl(elems, Nil)

    def parseDecl(sexp: SExpr): Declaration = sexp match
        // Declaration: (def Variable Expression)
        case SList(SSymbol(Keyword.Def) :: lhs :: rhs :: Nil) =>
            Declaration.Def(
                parseVar(lhs),
                parseExpr(rhs)
            )
        case _ => Declaration.Err(DeclErr.Malformed)

    def parseStmt(sexpr: SExpr): Statement = sexpr match
        case SList(SSymbol(Keyword.Def) :: _) =>
            Statement.Err(StmtErr.DeclAtStmtPosition)

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
        // Many: (block Declaration^* Statement^+)
        case SList(SSymbol(Keyword.Block) :: Nil) => 
            Block.Err(BlockErr.ManyNoStmts)
        case SList(SSymbol(Keyword.Block) :: elems) => {
            splitDeclsAndStmts(elems) match
                case (decls, Nil) => 
                    Block.Err(BlockErr.ManyNoStmts)
                case (decls, stmts) => 
                    Block.Many(
                        decls.map(parseDecl),
                        stmts.map(parseStmt)
                    )
        }
            
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
                case Keyword.Plus =>  Expression.BinOpExpr(v1, BinOp.Add, v2)
                case Keyword.Div  =>  Expression.BinOpExpr(v1, BinOp.Div, v2)
                case Keyword.Eq   =>  Expression.BinOpExpr(v1, BinOp.Equals, v2)
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
        

