import * as path from "path";
import { spawn } from "child_process";
import { workspace, ExtensionContext, window } from "vscode";

import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  StreamInfo,
  Trace,
} from "vscode-languageclient/node";

let client: LanguageClient;
let outputChannel = window.createOutputChannel("Toy LSP");
let traceChannel = window.createOutputChannel("Toy LSP Trace");

export function activate(context: ExtensionContext) {
  outputChannel.appendLine("[client] activating extension");
  const serverJar = context.asAbsolutePath(
    path.join("..", "scala_lsp_server", "out", "lsp-server.jar")
  );
  const serverCommand = "java";
  const serverArgs = ["-jar", serverJar];

  // If the extension is launched in debug mode then the debug server options are used
  // Otherwise the run options are used
  const serverCwd = context.asAbsolutePath(path.join("..", "scala_lsp_server"));
  const serverOptions: ServerOptions = (): Promise<StreamInfo> => {
    const child = spawn(serverCommand, serverArgs, { cwd: serverCwd });
    child.stderr.on("data", (data) => {
      const lines = data.toString().split(/\r?\n/);
      for (const line of lines) {
        if (line.trim().length > 0) {
          outputChannel.appendLine(`[server] ${line}`);
        }
      }
    });
    child.on("exit", (code, signal) => {
      outputChannel.appendLine(
        `[server] exited with code ${code ?? "null"} signal ${signal ?? "null"}`
      );
    });
    return Promise.resolve({
      reader: child.stdout,
      writer: child.stdin,
    });
  };
  outputChannel.appendLine(
    `[client] launching server: ${serverCommand} ${serverArgs.join(" ")}`
  );

  // Options to control the language client
  const clientOptions: LanguageClientOptions = {
    // Register the server for all documents by default
    documentSelector: [{ scheme: "file", language: "*" }],
    traceOutputChannel: traceChannel,
    synchronize: {
      // Notify the server about file changes to '.clientrc files contained in the workspace
      fileEvents: workspace.createFileSystemWatcher("**/.clientrc"),
    },
  };

  // Create the language client and start the client.
  client = new LanguageClient(
    "REPLACE_ME language-server-id",
    "REPLACE_ME language server name",
    serverOptions,
    clientOptions
  );

  // Start the client. This will also launch the server
  outputChannel.appendLine("[client] starting language client");
  client.start();
  client
    .setTrace(Trace.Verbose)
    .then(() => outputChannel.appendLine("[client] trace enabled"))
    .catch((error) =>
      outputChannel.appendLine(`[client] trace enable failed: ${error}`)
    );
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  outputChannel.appendLine("[client] stopping language client");
  return client.stop();
}
