package ast

// Program    ::= (Statement^* Expression)
case class Program(stmts: List[Statement], expr: Expression)

//   Statement  ::= (Variable = Expression)
//                | (if0 Expression Block Block)
//                | (while0 Expression Block)
enum Statement:
    case Assign(rhs: Expression.Var, lhs: Expression)
    case Ifelse(guard: Expression, tbranch: Block, ebranch: Block)
    case While(gurad: Expression, body: Block)

//   Block      ::= Statement
//                | (block Statement^+)
enum Block:
    case One(stmt: Statement)
    case Many(stmts: List[Statement])

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