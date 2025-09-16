package ast

import Error._

// Program    ::= (Statement^* Expression)
enum Program:
    case Prog(stmts: List[Statement], expr: Expression)
    case Err(e: ProgErr)

//   Statement  ::= (Variable = Expression)
//                | (if0 Expression Block Block)
//                | (while0 Expression Block)
enum Statement:
    case Assign(rhs: Expression.Var, lhs: Expression)
    case Ifelse(guard: Expression, tbranch: Block, ebranch: Block)
    case While(guard: Expression, body: Block)
    case Err(e: StmtErr)

//   Block      ::= Statement
//                | (block Statement^+)
enum Block:
    case One(stmt: Statement)
    case Many(stmts: List[Statement])
    case Err(e: BlockErr)

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