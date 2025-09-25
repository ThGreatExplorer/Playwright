package error

trait Error

class Unreachable(msg: String) extends Exception(msg)
class UnreachableStateException(msg: String) extends Exception(msg)
class UnloadedNonFinalStateException(msg: String) extends Exception(msg)