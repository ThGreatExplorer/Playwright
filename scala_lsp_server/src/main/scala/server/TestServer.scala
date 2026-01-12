package server

import java.util.concurrent.CompletableFuture
import org.eclipse.lsp4j.services.LanguageServer
import org.eclipse.lsp4j._
import org.eclipse.lsp4j.jsonrpc.ResponseErrorException
import org.eclipse.lsp4j.jsonrpc.messages.ResponseErrorCode
import org.eclipse.lsp4j.jsonrpc.messages.ResponseError
import org.eclipse.lsp4j.services.NotebookDocumentService
import org.eclipse.lsp4j.services.TextDocumentService
import org.eclipse.lsp4j.services.WorkspaceService
import org.eclipse.lsp4j.launch.LSPLauncher

class TestLanguageServer extends LanguageServer {

  private val textDocumentService = new TestTextDocumentService()
  private val workspaceService = new TestWorkspaceService()
  @volatile private var initializeReceived = false
  @volatile private var initializedReceived = false
  @volatile private var shutdownRequested = false

  /** The initialize request is sent as the first request from the client to the
    * server.
    *
    * If the server receives requests or notifications before the initialize
    * request, it should act as follows:
    *   - for a request, the response should be errored with:
    *     org.eclipse.lsp4j.jsonrpc.messages.ResponseErrorCode#ServerNotInitialized.
    *     The message can be picked by the server.
    *   - notifications should be dropped, except for the exit notification.
    *     This will allow the client to exit a server without an initialize
    *     request.
    *
    * Until the server has responded to the initialize request with an
    * InitializeResult, the client must not send any additional requests or
    * notifications to the server.
    *
    * During the initialize request, the server is allowed to send the
    * notifications window/showMessage, window/logMessage, and telemetry/event,
    * as well as the request window/showMessageRequest, to the client.
    */
  override def initialize(
      params: InitializeParams
  ): CompletableFuture[InitializeResult] = {
    ServerLogger.log("initialize request received")
    if (shutdownRequested) {
      return errorFuture(
        ResponseErrorCode.InvalidRequest.getValue,
        "Initialize request not allowed after shutdown."
      )
    }
    if (initializeReceived) {
      return errorFuture(
        ResponseErrorCode.InvalidRequest.getValue,
        "Initialize request already received."
      )
    }
    initializeReceived = true

    // Initialize capabilities
    val capabilities = new ServerCapabilities()
    capabilities.setTextDocumentSync(TextDocumentSyncKind.Full)

    // Return result
    val result = new InitializeResult(capabilities)
    ServerLogger.log("initialize response sent")
    CompletableFuture.completedFuture(result)
  }

  /** The initialized notification is sent from the client to the server after
    * the client received the result of the initialize request, but before the
    * client is sending any other request or notification to the server. The
    * server can use the initialized notification, for example, to dynamically
    * register capabilities.
    */
  override def initialized(params: InitializedParams): Unit = {
    ServerLogger.log("initialized notification received")
    if (!initializeReceived || initializedReceived) {
      return
    }
    initializedReceived = true
    ServerLogger.log("initialized state set")
  }

  /** The shutdown request is sent from the client to the server. It asks the
    * server to shutdown, but to not exit (otherwise the response might not be
    * delivered correctly to the client). There is a separate exit notification
    * that asks the server to exit.
    */
  override def shutdown(): CompletableFuture[AnyRef] = {
    ServerLogger.log("shutdown request received")
    if (!initializeReceived) {
      return errorFuture(ResponseErrorCode.ServerNotInitialized.getValue, "Shutdown before initialize.")
    }
    if (shutdownRequested) {
      return errorFuture(
        ResponseErrorCode.InvalidRequest.getValue,
        "Shutdown request already received."
      )
    }
    shutdownRequested = true
    ServerLogger.log("shutdown response sent")
    CompletableFuture.completedFuture(null)
  }

  /** A notification to ask the server to exit its process.
    */
  override def exit(): Unit = {
    ServerLogger.log("exit notification received")
    val exitCode = if (shutdownRequested) 0 else 1
    ServerLogger.log(s"exiting with code $exitCode")
    System.exit(exitCode)
  }

  /** Provides access to the textDocument services.
    */
  override def getTextDocumentService(): TextDocumentService = {
    textDocumentService
  }

  /** Provides access to the workspace services.
    */
  override def getWorkspaceService(): WorkspaceService = {
    workspaceService
  }

  /** Provides access to the notebookDocument services.
    *
    * @return
    *   NULL as we don't support notebooks
    */
  override def getNotebookDocumentService(): NotebookDocumentService = {
    null
  }

  /** This notification is sent from the client to the server to cancel a
    * progress initiated on the server side.
    */
  override def cancelProgress(params: WorkDoneProgressCancelParams): Unit = {
    // No-op
  }

  private def errorFuture[T](
      code: Int,
      message: String
  ): CompletableFuture[T] = {
    val future = new CompletableFuture[T]()
    val error = new ResponseError(code, message, null)
    future.completeExceptionally(new ResponseErrorException(error))
    future
  }
}
