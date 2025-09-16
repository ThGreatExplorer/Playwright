package ast

import Error._

// Program    ::= (Statement^* Expression)
enum Program:
    case Prog(stmts: List[Statement], expr: Expression)
    case Err(e: ProgErr)

    def hasError: Boolean =
        this match
            case Err(_) => true
            case Prog(stmts, expr) =>
                stmts.exists {
                    case Statement.Err(_) => true
                    case Statement.Assign(_, rhs) => rhs.hasError
                    case Statement.Ifelse(guard, tbranch, ebranch) =>
                        guard.hasError || tbranch.hasError || ebranch.hasError
                    case Statement.While(guard, body) =>
                        guard.hasError || body.hasError
                } || expr.hasError

//   Statement  ::= (Variable = Expression)
//                | (if0 Expression Block Block)
//                | (while0 Expression Block)
enum Statement:
    case Assign(rhs: Expression.Var, lhs: Expression)
    case Ifelse(guard: Expression, tbranch: Block, ebranch: Block)
    case While(guard: Expression, body: Block)
    case Err(e: StmtErr)

    def hasError: Boolean =
        this match
            case Err(_) => true
            case Assign(_, rhs) => rhs.hasError
            case Ifelse(guard, tbranch, ebranch) =>
                guard.hasError || tbranch.hasError || ebranch.hasError
            case While(guard, body) =>
                guard.hasError || body.hasError

//   Block      ::= Statement
//                | (block Statement^+)
enum Block:
    case One(stmt: Statement)
    case Many(stmts: List[Statement])
    case Err(e: BlockErr)

    def hasError: Boolean =
        this match
            case Err(_) => true
            case One(stmt) => stmt.hasError
            case Many(stmts) => stmts.exists(_.hasError)

//   Expression ::= GoodNumber
//                | Variable
//                | (Variable + Variable)
//                | (Variable / Variable)
//                | (Variable == Variable)
enum Expression:
    case Num(n: Double)
    case Var(x: String)
    case Add(lhs: Var, rhs: Var)
    case Div(lhs: Var, rhs: Var)
    case Equals(lhs: Var, rhs: Var)
    case Err(e: ExprErr)

    def hasError: Boolean =
        this match
            case Err(_) => true
            case _ => false