package server

import java.util.concurrent.CompletableFuture
import java.util.{List => JList}
import org.eclipse.lsp4j._
import org.eclipse.lsp4j.jsonrpc.messages.Either
import org.eclipse.lsp4j.services.WorkspaceService

/**
 * Scala skeleton for WorkspaceService with dummy implementations.
 */
class TestWorkspaceService extends WorkspaceService {

	// /**
	//  * The workspace/executeCommand request is sent from the client to the
	//  * server to trigger command execution on the server. In most cases the
	//  * server creates a WorkspaceEdit structure and applies the changes to the
	//  * workspace using the request workspace/applyEdit which is sent from the
	//  * server to the client.
	//  * Registration Options: org.eclipse.lsp4j.ExecuteCommandRegistrationOptions
	//  */
	// override def executeCommand(params: ExecuteCommandParams): CompletableFuture[AnyRef] =
	// 	throw new UnsupportedOperationException()

	// /**
	//  * The workspace symbol request is sent from the client to the server to
	//  * list project-wide symbols matching the query string.
	//  * Registration Options: org.eclipse.lsp4j.WorkspaceSymbolRegistrationOptions
	//  */
	// override def symbol(params: WorkspaceSymbolParams): CompletableFuture[Either[JList[_ <: SymbolInformation], JList[_ <: WorkspaceSymbol]]] =
	// 	throw new UnsupportedOperationException()

	// /**
	//  * The request is sent from the client to the server to resolve additional information
	//  * for a given workspace symbol.
	//  */
	// override def resolveWorkspaceSymbol(workspaceSymbol: WorkspaceSymbol): CompletableFuture[WorkspaceSymbol] =
	// 	throw new UnsupportedOperationException()

	/**
	 * A notification sent from the client to the server to signal the change of
	 * configuration settings.
	 */
	override def didChangeConfiguration(params: DidChangeConfigurationParams): Unit =
		()

	/**
	 * The watched files notification is sent from the client to the server when
	 * the client detects changes to file watched by the language client.
	 */
	override def didChangeWatchedFiles(params: DidChangeWatchedFilesParams): Unit =
		()

	// /**
	//  * The workspace/didChangeWorkspaceFolders notification is sent from the client
	//  * to the server to inform the server about workspace folder configuration changes.
	//  * The notification is sent by default if both ServerCapabilities/workspaceFolders
	//  * and ClientCapabilities/workspace/workspaceFolders are true; or if the server has
	//  * registered to receive this notification it first.
	//  */
	// override def didChangeWorkspaceFolders(params: DidChangeWorkspaceFoldersParams): Unit =
	// 	throw new UnsupportedOperationException()

	// /**
	//  * The will create files request is sent from the client to the server before files
	//  * are actually created as long as the creation is triggered from within the client.
	//  * The request can return a WorkspaceEdit which will be applied to workspace
	//  * before the files are created. Please note that clients might drop results if computing
	//  * the edit took too long or if a server constantly fails on this request. This is
	//  * done to keep creates fast and reliable.
	//  */
	// override def willCreateFiles(params: CreateFilesParams): CompletableFuture[WorkspaceEdit] =
	// 	throw new UnsupportedOperationException()

	// /**
	//  * The did create files notification is sent from the client to the server when files
	//  * were created from within the client.
	//  */
	// override def didCreateFiles(params: CreateFilesParams): Unit =
	// 	throw new UnsupportedOperationException()

	// /**
	//  * The will rename files request is sent from the client to the server before files
	//  * are actually renamed as long as the rename is triggered from within the client.
	//  * The request can return a WorkspaceEdit which will be applied to workspace
	//  * before the files are renamed. Please note that clients might drop results if computing
	//  * the edit took too long or if a server constantly fails on this request. This is
	//  * done to keep renames fast and reliable.
	//  */
	// override def willRenameFiles(params: RenameFilesParams): CompletableFuture[WorkspaceEdit] =
	// 	throw new UnsupportedOperationException()

	// /**
	//  * The did rename files notification is sent from the client to the server when files
	//  * were renamed from within the client.
	//  */
	// override def didRenameFiles(params: RenameFilesParams): Unit =
	// 	throw new UnsupportedOperationException()

	// /**
	//  * The will delete files request is sent from the client to the server before files
	//  * are actually deleted as long as the deletion is triggered from within the client.
	//  * The request can return a WorkspaceEdit which will be applied to workspace
	//  * before the files are deleted. Please note that clients might drop results if computing
	//  * the edit took too long or if a server constantly fails on this request. This is
	//  * done to keep deletes fast and reliable.
	//  */
	// override def willDeleteFiles(params: DeleteFilesParams): CompletableFuture[WorkspaceEdit] =
	// 	throw new UnsupportedOperationException()

	// /**
	//  * The did delete files notification is sent from the client to the server when files
	//  * were deleted from within the client.
	//  */
	// override def didDeleteFiles(params: DeleteFilesParams): Unit =
	// 	throw new UnsupportedOperationException()

	// /**
	//  * The workspace diagnostic request is sent from the client to the server to ask the server to
	//  * compute workspace wide diagnostics which previously where pushed from the server to the client.
	//  * In contrast to the document diagnostic request the workspace request can be long running and
	//  * is not bound to a specific workspace or document state. If the client supports streaming for
	//  * the workspace diagnostic pull it is legal to provide a document diagnostic report multiple times
	//  * for the same document URI. The last one reported will win over previous reports.
	//  */
	// override def diagnostic(params: WorkspaceDiagnosticParams): CompletableFuture[WorkspaceDiagnosticReport] =
	// 	throw new UnsupportedOperationException()

	// /**
	//  * The workspace/textDocumentContent request is sent from the client to the server to dynamically fetch
	//  * the content of a text document. Clients should treat the content returned from this request as read-only.
	//  * Registration Options: org.eclipse.lsp4j.TextDocumentContentRegistrationOptions
	//  */
	// override def textDocumentContent(params: TextDocumentContentParams): CompletableFuture[TextDocumentContentResult] =
	// 	throw new UnsupportedOperationException()
}