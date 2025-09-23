import ast.Error

class RuntimeError extends Error {}

enum DUMMY extends RuntimeError:
    case DivByZero
    case VarNotFound(x: String)
    case Other(msg: String)

class UnreachableStateError(val msg : String) extends RuntimeError {}