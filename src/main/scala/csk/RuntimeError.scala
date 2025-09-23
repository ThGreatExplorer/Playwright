package csk

import error.Error

enum RuntimeError extends Error:
  case VarNotFound(msg: String)
  case DivisionByZero(msg: String)