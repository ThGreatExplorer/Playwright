package ParserAST

import ast._
import sexprs.SExprs._

val keywords: List[SSymbol] = List(SSymbol("="), SSymbol("if0"), SSymbol("while0"), SSymbol("block"), SSymbol("/"), SSymbol("+"), SSymbol("=="))

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
                 // Assignment
                case SList(name :: SSymbol("=") :: expr :: Nil) =>
                    parseVar(name, Expression.Err(ExprErr.ExprBadVar)) match
                        // reassign the error to be a Statement Error
                        case Expression.Err(e) => Statement.Err(StmtErr.StmtAssignBadLHS)
                        case Expression.Var(v) => Statement.Assign(Expression.Var(v), parseExpr(expr))
                case SList(name :: SSymbol("=") :: Nil) =>
                    Statement.Err(StmtErr.StmtAssignBadRHS)
                case SList(SSymbol("=") :: _) =>
                    Statement.Err(StmtErr.StmtAssignBadLHS)

                // IfElse
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

                // While
                case SList(SSymbol("while0") :: grd :: body :: Nil) =>
                    Statement.While(
                        parseExpr(grd),
                        parseBlock(body)
                    )
                case SList(SSymbol("while0") :: grd :: Nil) => 
                    Statement.Err(StmtErr.StmtWhileNoBody)
                case SList(SSymbol("while0") :: Nil) =>
                    Statement.Err(StmtErr.StmtWhileNoGuard)

                case _ => Statement.Err(StmtErr.StmtFailedAssignIfWhileMatch)
        def parseBlock(sexpr: SExpr): Block = 
            sexpr match
                case SList(SSymbol("block") :: xs) =>
                    Block.Many(xs.map(parseStmt))
                case _ => 
                    Block.Err(BlockErr.BlockFailedOneManyBlockMatch)
        def parseExpr(sexpr: SExpr): Expression =
            sexpr match
                case SDouble(n) => Expression.Num(n)
                case SSymbol(s) => parseVar(SSymbol(s), Expression.Err(ExprErr.ExprBadVar))
                // Note that an Expression's operations can only be variables so 
                // strictly type check against SSymbols
                case SList(SSymbol(s_1) :: SSymbol(op) :: SSymbol(s_2) :: Nil) => 
                    (parseExpr(SSymbol(s_1)), parseExpr(SSymbol(s_2))) match
                        case (Expression.Var(v1), Expression.Var(v2)) => 
                            op match
                                case "+" => Expression.Add(Expression.Var(v1), Expression.Var(v2))
                                case "/" => Expression.Div(Expression.Var(v1), Expression.Var(v2))
                                case "==" => Expression.Equals(Expression.Var(v1), Expression.Var(v2))
                                case _ => Expression.Err(ExprErr.ExprBadOperand)
                        case (Expression.Err(_), Expression.Var(_)) => Expression.Err(ExprErr.ExprBadLHS)
                        case (Expression.Var(_), Expression.Err(_)) => Expression.Err(ExprErr.ExprBadRHS)
                        case _ => Expression.Err(ExprErr.ExprBadLHS)
                case _ => Expression.Err(ExprErr.ExprFailedNumVarAddDivEqualsMatch)
        def parseVar(ssymbol: SExpr, err: Expression.Err): Expression.Var | Expression.Err = 
            // make sure its not a keyword
            ssymbol match
                case SSymbol(s) => if !keywords.contains(SSymbol(s)) then Expression.Var(s) else err
                case _ => err

        parseProg(sexpr)
        

    




