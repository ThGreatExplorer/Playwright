import ast._
import sexprs.SExprs._

object Parser:

    def hasError(p: Program): Boolean = true

    def parse(sexpr: SExpr): Program =
        def parseProg(sexpr: SExpr): Program =
            sexpr match
                case SList(Nil) => Program.Err(ProgErr.ProgEmptyList)
                case SList(elems) => {
                    val stmts = elems.dropRight(1)
                    val expr = elems.last
                    Program.Prog(
                        stmts.map(parseStmt),
                        parseExpr(expr)
                    )
                }
                case _ => Program.Err(ProgErr.ProgNotList)
        def parseStmt(sexpr: SExpr): Statement = 
            sexpr match
                case SList(name :: SSymbol("=") :: expr :: Nil) =>
                    Statement.Assign(
                        parseVar(name), 
                        parseExpr(expr)
                    )
                case SList(SSymbol("if0") :: grd :: thn :: els :: Nil) =>
                    Statement.Ifelse(
                        parseExpr(grd),
                        parseBlock(thn),
                        parseBlock(els)
                    )
                case SList(SSymbol("while0") :: grd :: body :: Nil) =>
                    Statement.While(
                        parseExpr(grd),
                        parseBlock(body)
                    )
                case _ => Statement.Err(StmtErr.StmtAssignBadRHS)
        def parseBlock(sexpr: SExpr): Block = 
            sexpr match
                case SList(SSymbol("block") :: xs) =>
                    Block.Many(xs.map(parseStmt))
                case _ => 
                    Block.Err(BlockErr.BlockFailedOneManyBlockMatch)
        def parseExpr(sexpr: SExpr): Expression = 
            Expression.Err(ExprErr.ExprBadNumber)
        def parseVar(sexpr: SExpr): Expression.Var = 
            // make sure its not a keyword
            Expression.Var("s")
                   
        parseProg(sexpr)
        

    




