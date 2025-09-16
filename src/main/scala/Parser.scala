import ast._
import sexprs.SExprs._

object Parser:

    def hasError(p: Program): Boolean = 
        return p.hasError

    def parse(sexpr: SExpr): Program =
        def parseProg(sexpr: SExpr): Program =
            sexpr match
                case SList(SList(stmts) :: expr :: Nil) => 
                    Program.Prog(
                        stmts.map(parseStmt), 
                        parseExpr(expr)
                    )
                case _ => Program.Err()
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
                case _ => Statement.Err()
        def parseBlock(sexpr: SExpr): Block = 
            sexpr match
                case SList(SSymbol("block") :: xs)   =>
                    Block.
                case SList(SSymbol("while0") :: grd :: body :: Nil) =>
                    Statement.While(
                        parseExpr(grd),
                        parseBlock(body)
                    )
                case _ => Statement.Err()
            

        

        parseProg(sexpr)
        

    




