package server

object ServerLogger {
  private val prefix = "[lsp-server]"

  def log(message: String): Unit =
    System.err.println(s"$prefix $message")
}
