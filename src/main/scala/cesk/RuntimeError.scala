package cesk

enum RuntimeError:
  case VarNotFound(msg: String)
  case DivisionByZero(msg: String)
