package ast

trait Error {}

class ParserError extends Error {}

enum ProgErr extends ParserError:
    case ProgNotList
    case ProgEmptyList

enum StmtErr extends ParserError:
    case StmtAssignBadLHS
    case StmtAssignBadRHS
    case StmtIfelseNoGuard
    case StmtIfelseNoTBranch
    case StmtIfelseNoEBranch
    case StmtWhileNoGuard
    case StmtWhileNoBody
    case StmtMalformed

enum BlockErr extends ParserError:
    case BlockManyNoStmts

enum ExprErr extends ParserError:
    case ExprBadNumber
    case ExprVarIsKeyword
    case ExprBadVar
    case ExprBadOperand
    case ExprMalformed