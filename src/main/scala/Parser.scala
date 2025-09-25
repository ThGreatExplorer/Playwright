package ParserAST

import annotation.tailrec

import ast._
import sexprs.SExprs._
import ExampleBB.bbKeywords

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
                case SList(Nil) => Program.Err(ProgErr.ProgEmptyList)
                case SList(elems) => {
                    val stmts = elems.init
                    val expr = elems.last
                    Program.Prog(
                        parseStmtsTail(stmts),
                        parseExpr(expr)
                    )
                }
                case _ => Program.Err(ProgErr.ProgNotList)

        // use tail-recursion for parsing lists of statements to avoid stack overflow
        // map uses recursion for Lists under the hood
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
                case SList(SSymbol("=") :: _) =>
                    Statement.Err(StmtErr.StmtAssignBadLHS)
                case SList(name :: SSymbol("=") :: _) =>
                    Statement.Err(StmtErr.StmtAssignBadRHS)
                
                // IfElse: (if0 Expression Block Block)
                case SList(SSymbol("if0") :: grd :: thn :: els :: Nil) =>
                    Statement.Ifelse(
                        parseExpr(grd),
                        parseBlock(thn),
                        parseBlock(els)
                    )
                case SList(SSymbol("if0") :: grd :: thn :: Nil) =>
                    Statement.Err(StmtErr.StmtIfelseNoEBranch)
                case SList(SSymbol("if0") :: grd :: Nil) =>
                    Statement.Err(StmtErr.StmtIfelseNoTBranch)
                case SList(SSymbol("if0") :: Nil) =>
                    Statement.Err(StmtErr.StmtIfelseNoGuard)

                // While: (while0 Expression Block)
                case SList(SSymbol("while0") :: grd :: body :: Nil) =>
                    Statement.While(
                        parseExpr(grd),
                        parseBlock(body)
                    )
                case SList(SSymbol("while0") :: grd :: Nil) => 
                    Statement.Err(StmtErr.StmtWhileNoBody)
                case SList(SSymbol("while0") :: Nil) =>
                    Statement.Err(StmtErr.StmtWhileNoGuard)

                case _ => Statement.Err(StmtErr.StmtMalformed)
        def parseBlock(sexpr: SExpr): Block = 
            sexpr match
                // Many: (block Statement^+)
                case SList(SSymbol("block") :: Nil) => 
                    Block.Err(BlockErr.BlockManyNoStmts)
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
                        case _ =>    Expression.Err(ExprErr.ExprBadOperand)
                }
                case _ => 
                    Expression.Err(ExprErr.ExprMalformed)
        def parseVar(ssymbol: SExpr): Expression.Var | Expression.Err = 
            ssymbol match
                // Var: the set of Variables consists of all symboSls, minus keywords
                case SSymbol(s) => 
                    if !bbKeywords.contains(s) then
                        Expression.Var(s) 
                    else 
                        Expression.Err(ExprErr.ExprVarIsKeyword)
                case _ => Expression.Err(ExprErr.ExprBadVar)
        parseProg(sexpr)
        

    /** Walks the AST and determines if any of the nodes contain an error. 
      * Short-circuits.
      * 
      * @param p BareBones AST
      * @return true if the AST contains error nodes
      */
    def hasError(p: Program): Boolean = 
        def stmtHasError(s: Statement): Boolean =
            s match
                case Statement.Err(_) => true
                case Statement.Assign(lhs, rhs) => exprHasError(lhs) || exprHasError(rhs)
                case Statement.Ifelse(guard, tbranch, ebranch) =>
                    exprHasError(guard) || blockHasError(tbranch) || blockHasError(ebranch)
                case Statement.While(guard, body) =>
                    exprHasError(guard) || blockHasError(body)
        def blockHasError(b: Block): Boolean =
            b match
                case Block.Err(_) => true
                case Block.One(stmt) => stmtHasError(stmt)
                // use an iterative approach to avoid stack overflow as exists is 
                // recursively implemented on Lists
                case Block.Many(stmts) => stmts.iterator.exists(s => stmtHasError(s))
        def exprHasError(e: Expression): Boolean = 
            e match
                case Expression.Err(_) => true
                case Expression.Add(lhs, rhs) => exprHasError(lhs) || exprHasError(rhs)
                case Expression.Div(lhs, rhs) => exprHasError(lhs) || exprHasError(rhs)
                case Expression.Equals(lhs, rhs) => exprHasError(lhs) || exprHasError(rhs)
                case _ => false
        p match
            case Program.Err(_) => true
            case Program.Prog(stmts, expr) =>
                stmts.iterator.exists {s => stmtHasError(s)} || exprHasError(expr)




