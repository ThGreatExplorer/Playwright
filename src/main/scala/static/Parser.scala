package static

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
    def parseProg(sexpr: SExpr): ProgramWE = sexpr match
        // Program: (Declaration^* Statement^* Expression)  
        case SList(Nil) => ProgramWE.Err(ProgErr.EmptyList) 
        case SList(elems) => {
            val (decls, stmts) = splitDeclsAndStmts(elems.dropRight(1))
            val expr = elems.last
            ProgramWE.Prog(
                decls.map(parseDecl),
                stmts.map(parseStmt),
                parseExpr(expr)
            )
        }
        case _ => ProgramWE.Err(ProgErr.NotAList)

    def splitDeclsAndStmts(elems: List[SExpr]) : (List[SExpr], List[SExpr]) = 
        def loopDecl(remaining: List[SExpr], accDecls: List[SExpr]) : (List[SExpr], List[SExpr]) =
            remaining match
                case Nil => (accDecls.reverse, Nil)
                case (head @ SList(SSymbol(Keyword.Def) :: _)) :: rest => 
                    loopDecl(rest, head :: accDecls)
                case _ => (accDecls.reverse, remaining)
        loopDecl(elems, Nil)

    def parseDecl(sexp: SExpr): DeclWE = sexp match
        // Declaration: (def Variable Expression)
        case SList(SSymbol(Keyword.Def) :: lhs :: rhs :: Nil) =>
            DeclWE.Def(
                parseVar(lhs),
                parseExpr(rhs)
            )
        case _ => DeclWE.Err(DeclErr.Malformed)

    def parseStmt(sexpr: SExpr): StmtWE = sexpr match
        case SList(SSymbol(Keyword.Def) :: _) =>
            StmtWE.Err(StmtErr.DeclAtStmtPosition)

        // Assignment: (Variable = Expression)
        case SList(name :: SSymbol(Keyword.Assign) :: expr :: Nil) =>
            StmtWE.Assign(
                parseVar(name), 
                parseExpr(expr)
            )
        case SList(name :: SSymbol(Keyword.Assign) :: _) =>
            StmtWE.Err(StmtErr.AssignRhsMalformed)
        
        // IfElse: (if0 Expression Block Block)
        case SList(SSymbol(Keyword.If) :: grd :: thn :: els :: Nil) =>
            StmtWE.Ifelse(
                parseExpr(grd),
                parseBlock(thn),
                parseBlock(els)
            )
        case SList(SSymbol(Keyword.If) :: _) =>
            StmtWE.Err(StmtErr.IfelseMalformed)

        // While: (while0 Expression Block)
        case SList(SSymbol(Keyword.While) :: grd :: body :: Nil) =>
            StmtWE.While(
                parseExpr(grd),
                parseBlock(body)
            )
        case SList(SSymbol(Keyword.While) :: _) =>
            StmtWE.Err(StmtErr.WhileMalformed)

        case _ => StmtWE.Err(StmtErr.Malformed)

    def parseBlock(sexpr: SExpr): BlockWE = sexpr match
        // Many: (block Declaration^* Statement^+)
        case SList(SSymbol(Keyword.Block) :: Nil) => 
            BlockWE.Err(BlockErr.ManyNoStmts)
        case SList(SSymbol(Keyword.Block) :: elems) => {
            splitDeclsAndStmts(elems) match
                case (decls, Nil) => 
                    BlockWE.Err(BlockErr.ManyNoStmts)
                case (decls, stmts) => 
                    BlockWE.Many(
                        decls.map(parseDecl),
                        stmts.map(parseStmt)
                    )
        }
            
        // One: Statement
        case _ => 
            BlockWE.One(parseStmt(sexpr))

    def parseExpr(sexpr: SExpr): ExprWE = sexpr match
        // Num: the set of GoodNumbers comprises all inexact numbers
        //      (doubles) between -1000.0 and +1000.0, inclusive.
        case SDouble(n) => 
            ExprWE.Num(n)
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
                case Keyword.Plus =>  ExprWE.BinOpExpr(v1, BinOp.Add, v2)
                case Keyword.Div  =>  ExprWE.BinOpExpr(v1, BinOp.Div, v2)
                case Keyword.Eq   =>  ExprWE.BinOpExpr(v1, BinOp.Equals, v2)
                case _            =>  ExprWE.Err(ExprErr.BadOperand)
        }
        case _ => 
            ExprWE.Err(ExprErr.Malformed)

    def parseVar(ssymbol: SExpr): VarWE = ssymbol match
        // Var: the set of Variables consists of all symboSls, minus keywords
        case SSymbol(s) => 
            if !isKeyword(s) then
                ExprWE.Var(s) 
            else 
                ExprWE.VarErrNode(VarErr.IsKeyword)
        case _ => ExprWE.VarErrNode(VarErr.NotAName)
        

