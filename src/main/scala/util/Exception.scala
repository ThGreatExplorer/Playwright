package util

/**
  * These Execeptions are used to signal that something went horribly wrong.
  * Only to be used when:
  * - An input sexpr is not an Example as per spec
  * - A pattern match is not reachable  
  *
  * @param msg A description of the problem
  */
class InputNotExampleException(msg: String) extends Exception(msg)
class UnreachablePatternMatch(msg: String) extends Exception(msg)
class UnreachableStateException(msg: String) extends Exception(msg)
class UnloadedNonFinalStateException(msg: String) extends Exception(msg)