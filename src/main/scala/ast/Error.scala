package ast

enum ParseErr:
    // Program
    case ProgNotList
    case ProgNoStmts
    case ProgNoExpr

