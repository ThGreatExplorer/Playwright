package ast

enum ParseErrNodes:
    case ShapeMalformed
    case FieldTypeMalformed
    case MethodTypeMalformed
    // System
    case SystemNotAList
    case SystemEmptyList
    // Module
    case ModuleMalformed
    // Import
    case ImportMalformed
    // Prog
    case ProgNotAList
    case ProgEmptyList
    // Class
    case ClassMalformed
    // Method
    case MethodMalformed
    // ProgBlock
    case ProgBlockNoExpr
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
    // Name duplicates in Class/Module definitons
    case DuplicateModuleName
    case DuplicateClassName
    case DuplicateFieldName
    case DuplicateMethod
    case DuplicateParamName
    // Var ref to an undeclared variable
    case VarNotDeclared
    // Import ref to an undeclared module
    case ModuleNotDeclared
    // Class ref to an undeclared class
    case ClassNotDeclared

enum TypeErrorNodes:
    case BinOpWithNonNumberType
    case StrongTypingViolation