package ast

enum ParseErrNodes:
    // Prog
    case ProgNotAList
    case ProgEmptyList
    // Class
    case ClassMalformed
    // Method
    case MethodMalformed
    case MethodNoExpr
    // Statement
    case AssignRhsMalformed
    case IfelseMalformed
    case WhileMalformed
    case StmtMalformed
    case DeclAtStmtPosition
    // Decl
    case DeclMalformed
    // Block
    case BlockManyNoStmts
    // Expr 
    case ExprBadOperand
    case ExprMalformed
    // Name
    case NameIsKeyword
    case NotAName

enum ValidityErrNodes:
    // Name duplicates in Class definitons
    case DuplicateClassName
    case DuplicateFieldName
    case DuplicateMethod
    case DuplicateParamName
    // Var ref to an undeclared variable
    case VarNotDeclared
    // Class ref to an undeclared class
    case ClassNotDeclared