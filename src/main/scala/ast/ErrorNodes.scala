package ast

enum ProgErr:
    case NotAList
    case EmptyList

enum StmtErr:
    case AssignRhsMalformed
    case IfelseMalformed
    case WhileMalformed
    case Malformed
    case DeclAtStmtPosition

enum DeclErr:
    case Malformed

enum BlockErr:
    case ManyNoStmts

enum ExprErr:
    case BadOperand
    case Malformed
    
enum VarErr:
    case IsKeyword
    case NotAName
    // The only error node added for Validity checking
    case NotDeclared