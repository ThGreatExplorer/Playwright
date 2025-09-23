import ast.Error

class RuntimeError extends Error {}

class VarNotFoundError(msg: String) extends RuntimeError {}
class UnreachableStateError(val msg : String) extends RuntimeError {}