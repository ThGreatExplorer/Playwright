import ast._
import sexprs.SExprs._

object Parser:

    def hasError(p: Program): Boolean = 
        def stmtHasError(s: Statement): Boolean =
            s match
                case Statement.Err(_) => true
                case Statement.Assign(_, lhs) => exprHasError(lhs)
                case Statement.Ifelse(guard, tbranch, ebranch) =>
                    exprHasError(guard) || blockHasError(tbranch) || blockHasError(ebranch)
                case Statement.While(guard, body) =>
                    exprHasError(guard) || blockHasError(body)

        def blockHasError(b: Block): Boolean =
            b match
                case Block.Err(_) => true
                case Block.One(stmt) => stmtHasError(stmt)
                case Block.Many(stmts) => stmts.exists(s => stmtHasError(s))
        
        def exprHasError(e: Expression): Boolean = 
            e match
                case Expression.Err(_) => true
                case _ => false
        
        p match
            case Program.Err(_) => true
            case Program.Prog(stmts, expr) =>
                stmts.exists {s => stmtHasError(s)} || exprHasError(expr)

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
        

    




