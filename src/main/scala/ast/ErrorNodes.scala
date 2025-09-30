package ast

enum ProgErr:
    case NotAList
    case EmptyList

enum StmtErr:
    case AssignRhsMalformed
    case IfelseMalformed
    case WhileMalformed
    case Malformed

enum BlockErr:
    case ManyNoStmts

enum ExprErr:
    case VarIsKeyword
    case VarNotAName
    case BadOperand
    case Malformed
    // The only error node added for Validity checking
    case ExprVarNotDeclared