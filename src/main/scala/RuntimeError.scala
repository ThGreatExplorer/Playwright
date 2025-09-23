import ast.Error

enum RuntimeError extends Error {
  case VarNotFound(msg: String)
  case UnreachableState(msg: String)
  case DivisionByZero(msg: String)
}