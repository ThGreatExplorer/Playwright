package parser

import annotation.tailrec

import ast._
import sexprs.SExprs._
import util.ccKeywords

object Parser:

    /** Parses the given sexpr into a BareBones program to the best of its ability.
      * If grammar is invalid, an error node is inserted instead.
      *
      * @param sexpr SExpr read in from input
      * @return BareBones AST with possible error nodes if grammar rules are violated
      */
    def parse(sexpr: SExpr): Program =
        def parseProg(sexpr: SExpr): Program =
            sexpr match
                case SList(Nil) => Program.Err(ProgErr.EmptyList)
                case SList(elems) => {
                    val stmts = elems.init
                    val expr = elems.last
                    Program.Prog(
                        parseStmtsTail(stmts),
                        parseExpr(expr)
                    )
                }
                case _ => Program.Err(ProgErr.NotAList)

        // Use tail-recursion for parsing lists of statements to avoid stack overflow
        // map() uses recursion for Lists under the hood
        def parseStmtsTail(stmts: List[SExpr]): List[Statement] =
            @tailrec
            def loop(remaining: List[SExpr], acc: List[Statement]): List[Statement] =
                remaining match
                    case Nil => acc.reverse
                    case h :: t => loop(t, parseStmt(h) :: acc)
            loop(stmts, Nil)

        def parseStmt(sexpr: SExpr): Statement = 
            sexpr match
                 // Assignment: (Variable = Expression)
                case SList(name :: SSymbol("=") :: expr :: Nil) =>
                    Statement.Assign(
                        parseVar(name), 
                        parseExpr(expr)
                    )
                case SList(name :: SSymbol("=") :: _) =>
                    Statement.Err(StmtErr.AssignRhsMalformed)
                
                // IfElse: (if0 Expression Block Block)
                case SList(SSymbol("if0") :: grd :: thn :: els :: Nil) =>
                    Statement.Ifelse(
                        parseExpr(grd),
                        parseBlock(thn),
                        parseBlock(els)
                    )
                case SList(SSymbol("if0") :: _) =>
                    Statement.Err(StmtErr.IfelseMalformed)

                // While: (while0 Expression Block)
                case SList(SSymbol("while0") :: grd :: body :: Nil) =>
                    Statement.While(
                        parseExpr(grd),
                        parseBlock(body)
                    )
                case SList(SSymbol("while0") :: _) =>
                    Statement.Err(StmtErr.WhileMalformed)

                case _ => Statement.Err(StmtErr.Malformed)
        def parseBlock(sexpr: SExpr): Block = 
            sexpr match
                // Many: (block Statement^+)
                case SList(SSymbol("block") :: Nil) => 
                    Block.Err(BlockErr.ManyNoStmts)
                case SList(SSymbol("block") :: xs) =>
                    Block.Many(parseStmtsTail(xs))
                // One: Statement
                case _ => 
                    Block.One(parseStmt(sexpr))
        def parseExpr(sexpr: SExpr): Expression =
            sexpr match
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
                        case "+" =>  Expression.Add(v1, v2)
                        case "/" =>  Expression.Div(v1, v2)
                        case "==" => Expression.Equals(v1, v2)
                        case _ =>    Expression.Err(ExprErr.BadOperand)
                }
                case _ => 
                    Expression.Err(ExprErr.Malformed)
        def parseVar(ssymbol: SExpr): Expression.Var | Expression.Err = 
            ssymbol match
                // Var: the set of Variables consists of all symboSls, minus keywords
                case SSymbol(s) => 
                    if !ccKeywords.contains(s) then
                        Expression.Var(s) 
                    else 
                        Expression.Err(ExprErr.VarIsKeyword)
                case _ => Expression.Err(ExprErr.VarNotAName)
        parseProg(sexpr)
        

