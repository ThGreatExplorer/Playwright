package csk

import error.Error

enum RuntimeError extends Error:
  case VarNotFound(msg: String)
  case DivisionByZero(msg: String)

class UnreachableStateException(msg: String) extends Exception(msg)
class UnloadedNonFinalStateException(msg: String) extends Exception(msg)