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
    case StmtFailedAssignIfWhileMatch

enum BlockErr extends ParserError:
    case BlockManyNoStmts
    case BlockOneNoStmt
    case BlockFailedOneManyBlockMatch

enum ExprErr extends ParserError:
    case ExprBadNumber
    case ExprBadVar
    case ExprBadLHS
    case ExprBadRHS
    case ExprBadOperand
    case ExprFailedNumVarAddDivEqualsMatch