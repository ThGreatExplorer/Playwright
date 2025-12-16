package server

import org.eclipse.lsp4j.services.TextDocumentService
import org.eclipse.lsp4j.DidChangeTextDocumentParams
import org.eclipse.lsp4j.DidCloseTextDocumentParams
import org.eclipse.lsp4j.DidOpenTextDocumentParams
import org.eclipse.lsp4j.DidSaveTextDocumentParams
import org.eclipse.lsp4j.TextDocumentItem
import scala.collection.mutable.Map as MutableMap

class TestTextDocumentService extends TextDocumentService {

  private val openDocuments = MutableMap[String, TextDocumentItem]()

  // // The Completion request is sent from the client to the server to compute completion items at a given cursor position.
  // override def completion(position: CompletionParams): CompletableFuture[Either[JList[CompletionItem], CompletionList]] =
  // 	throw new UnsupportedOperationException()

  // // The request is sent from the client to the server to resolve additional information for a given completion item.
  // override def resolveCompletionItem(unresolved: CompletionItem): CompletableFuture[CompletionItem] =
  // 	throw new UnsupportedOperationException()

  // // The hover request is sent from the client to the server to request hover information at a given text document position.
  // override def hover(params: HoverParams): CompletableFuture[Hover] =
  // 	throw new UnsupportedOperationException()

  // // The signature help request is sent from the client to the server to request signature information at a given cursor position.
  // override def signatureHelp(params: SignatureHelpParams): CompletableFuture[SignatureHelp] =
  // 	throw new UnsupportedOperationException()

  // // The go to declaration request is sent from the client to the server to resolve the declaration location of a symbol at a given text document position.
  // override def declaration(params: DeclarationParams): CompletableFuture[Either[JList[_ <: Location], JList[_ <: LocationLink]]] =
  // 	throw new UnsupportedOperationException()

  // // The goto definition request is sent from the client to the server to resolve the definition location of a symbol at a given text document position.
  // override def definition(params: DefinitionParams): CompletableFuture[Either[JList[_ <: Location], JList[_ <: LocationLink]]] =
  // 	throw new UnsupportedOperationException()

  // // The goto type definition request is sent from the client to the server to resolve the type definition location of a symbol at a given text document position.
  // override def typeDefinition(params: TypeDefinitionParams): CompletableFuture[Either[JList[_ <: Location], JList[_ <: LocationLink]]] =
  // 	throw new UnsupportedOperationException()

  // // The goto implementation request is sent from the client to the server to resolve the implementation location of a symbol at a given text document position.
  // override def implementation(params: ImplementationParams): CompletableFuture[Either[JList[_ <: Location], JList[_ <: LocationLink]]] =
  // 	throw new UnsupportedOperationException()

  // // The references request is sent from the client to the server to resolve project-wide references for the symbol denoted by the given text document position.
  // override def references(params: ReferenceParams): CompletableFuture[JList[_ <: Location]] =
  // 	throw new UnsupportedOperationException()

  // // The document highlight request is sent from the client to the server to resolve document highlights for a given text document position.
  // override def documentHighlight(params: DocumentHighlightParams): CompletableFuture[JList[_ <: DocumentHighlight]] =
  // 	throw new UnsupportedOperationException()

  // // The document symbol request is sent from the client to the server to list all symbols found in a given text document.
  // override def documentSymbol(params: DocumentSymbolParams): CompletableFuture[JList[Either[SymbolInformation, DocumentSymbol]]] =
  // 	throw new UnsupportedOperationException()

  // // The code action request is sent from the client to the server to compute commands for a given text document and range.
  // override def codeAction(params: CodeActionParams): CompletableFuture[JList[Either[Command, CodeAction]]] =
  // 	throw new UnsupportedOperationException()

  // // The request is sent from the client to the server to resolve additional information for a given code action.
  // override def resolveCodeAction(unresolved: CodeAction): CompletableFuture[CodeAction] =
  // 	throw new UnsupportedOperationException()

  // // The code lens request is sent from the client to the server to compute code lenses for a given text document.
  // override def codeLens(params: CodeLensParams): CompletableFuture[JList[_ <: CodeLens]] =
  // 	throw new UnsupportedOperationException()

  // // The code lens resolve request is sent from the client to the server to resolve the command for a given code lens item.
  // override def resolveCodeLens(unresolved: CodeLens): CompletableFuture[CodeLens] =
  // 	throw new UnsupportedOperationException()

  // // The document formatting request is sent from the client to the server to format a whole document.
  // override def formatting(params: DocumentFormattingParams): CompletableFuture[JList[_ <: TextEdit]] =
  // 	throw new UnsupportedOperationException()

  // // The document range formatting request is sent from the client to the server to format a given range in a document.
  // override def rangeFormatting(params: DocumentRangeFormattingParams): CompletableFuture[JList[_ <: TextEdit]] =
  // 	throw new UnsupportedOperationException()

  // // The document on type formatting request is sent from the client to the server to format parts of the document during typing.
  // override def onTypeFormatting(params: DocumentOnTypeFormattingParams): CompletableFuture[JList[_ <: TextEdit]] =
  // 	throw new UnsupportedOperationException()

  // // The rename request is sent from the client to the server to do a workspace wide rename of a symbol.
  // override def rename(params: RenameParams): CompletableFuture[WorkspaceEdit] =
  // 	throw new UnsupportedOperationException()

  // The document open notification is sent from the client to the server to signal newly opened text documents.
  override def didOpen(params: DidOpenTextDocumentParams): Unit =
    val doc = params.getTextDocument
    openDocuments(doc.getUri) = doc

    // TODO: Add validation (parsing + validity checking + type-checking), this will require updating AST to track position
    // TODO: Add indexing, which will be a pass that traverses the AST that creates a map (for each symbol: variable, function, class, etc.) to ASTNode + source position with scoping? Design this appropriately
    ()

  // The document change notification is sent from the client to the server to signal changes to a text document.
  override def didChange(params: DidChangeTextDocumentParams): Unit =
    ()

  // The document close notification is sent from the client to the server when the document got closed in the client.
  override def didClose(params: DidCloseTextDocumentParams): Unit =
    val uri = params.getTextDocument.getUri
    openDocuments.remove(uri)
    ()

  // The document save notification is sent from the client to the server when the document is saved in the client.
  override def didSave(params: DidSaveTextDocumentParams): Unit =
    ()
    
  // TODO: Implement this validate text document
  private def validateTextDocument(): Unit =
    ()
}