package ast

enum ProgErr:
    case ProgNotList
    case ProgEmptyList

enum StmtErr:
    case StmtAssignBadLHS
    case StmtAssignBadRHS
    case StmtIfelseNoGuard
    case StmtIfelseNoTBranch
    case StmtIfelseNoEBranch
    case StmtWhileNoGuard
    case StmtWhileNoBody
    case StmtMalformed

enum BlockErr:
    case BlockManyNoStmts

enum ExprErr:
    case ExprVarIsKeyword
    case ExprBadVar
    case ExprBadOperand
    case ExprMalformed
    // The only error node added for Validity checking
    case ExprVarNotDeclared