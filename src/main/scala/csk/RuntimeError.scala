package csk

enum RuntimeError:
  case VarNotFound(msg: String)
  case DivisionByZero(msg: String)
