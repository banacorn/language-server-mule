// type method = ViaStdIO(string, string) | ViaTCP(int) | ViaPrebuilt(string, string)

module Error = {
  type t =
    // connection
    | ConnectionError(Js.Exn.t)
    | CannotSendRequest(Js.Exn.t)
    | CannotSendNotification(Js.Exn.t)
}
module LSP = LanguageServerMule.LSP

module type Module = {
  type t
  // lifecycle
  let make: (string, string, Handle.t) => Promise.t<result<t, Error.t>>
  let destroy: t => Promise.t<unit>
  // request / notification / error
  let sendRequest: (t, Js.Json.t) => Promise.t<result<Js.Json.t, Error.t>>
  let sendNotification: (t, Js.Json.t) => Promise.promise<result<unit, Error.t>>
  let onNotification: (Js.Json.t => unit) => VSCode.Disposable.t
  let onError: (Error.t => unit) => VSCode.Disposable.t
  // properties
  let getHandle: t => Handle.t
}

module Module: Module = {
  open VSCode

  type t = {
    client: LSP.LanguageClient.t,
    subscription: VSCode.Disposable.t,
    id: string, // language id, also for identifying custom methods
    name: string, // name for the language server client 
    handle: Handle.t,
  }

  // for emitting errors
  let errorChan: Chan.t<Js.Exn.t> = Chan.make()
  // for receiving notification
  let notificationChan: Chan.t<Js.Json.t> = Chan.make()

  let onError = callback =>
    errorChan->Chan.on(e => callback(Error.ConnectionError(e)))->VSCode.Disposable.make
  let onNotification = callback => notificationChan->Chan.on(callback)->VSCode.Disposable.make
  let sendNotification = (self, data) =>
    self.client
    ->LSP.LanguageClient.onReady
    ->Promise.Js.toResult
    ->Promise.mapOk(() => {
      self.client->LSP.LanguageClient.sendNotification(self.id, data)
    })
    ->Promise.mapError(exn => Error.CannotSendNotification(exn))

  let sendRequest = (self, data) =>
    self.client
    ->LSP.LanguageClient.onReady
    ->Promise.Js.toResult
    ->Promise.flatMapOk(() => {
      self.client->LSP.LanguageClient.sendRequest(self.id, data)->Promise.Js.toResult
    })
    ->Promise.mapError(exn => Error.CannotSendRequest(exn))

  let destroy = self => {
    self.subscription->VSCode.Disposable.dispose->ignore
    self.client->LSP.LanguageClient.stop->Promise.Js.toResult->Promise.map(_ => ())
  }

  let make = (id, name, handle) => {
    let serverOptions = switch handle {
    | Handle.TCP(port, _host) => LSP.ServerOptions.makeWithStreamInfo(port, "localhost")
    | StdIO(_name, path) => LSP.ServerOptions.makeWithCommand(path)
    // | ViaPrebuilt(_version, path) => LSP.ServerOptions.makeWithCommand(path)
    }

    let clientOptions = {
      // Register the server for plain text documents
      let documentSelector: DocumentSelector.t = [
        StringOr.others({
          open DocumentFilter
          {
            scheme: Some("file"),
            pattern: None,
            language: Some(id),
          }
        }),
      ]

      // Notify the server about file changes to '.clientrc files contained in the workspace
      let synchronize: FileSystemWatcher.t = Workspace.createFileSystemWatcher(
        %raw("'**/.clientrc'"),
        ~ignoreCreateEvents=false,
        ~ignoreChangeEvents=false,
        ~ignoreDeleteEvents=false,
      )

      let errorHandler: LSP.ErrorHandler.t = LSP.ErrorHandler.make(
        ~error=(exn, _msg, _count) => {
          errorChan->Chan.emit(exn)
          Shutdown
        },
        ~closed=() => {
          DoNotRestart
        },
      )
      LSP.LanguageClientOptions.make(documentSelector, synchronize, errorHandler)
    }

    // Create the language client
    let languageClient = LSP.LanguageClient.make(
      id,
      name,
      serverOptions,
      clientOptions,
    )

    let self = {
      client: languageClient,
      subscription: languageClient->LSP.LanguageClient.start,
      id: id,
      name: name,
      handle: handle,
    }

    // Let `LanguageClient.onReady` and `errorChan->Chan.once` race
    Promise.race(list{
      self.client->LSP.LanguageClient.onReady->Promise.Js.toResult,
      errorChan->Chan.once->Promise.map(err => Error(err)),
    })
    ->Promise.mapError(error => Error.ConnectionError(error))
    ->Promise.mapOk(() => {
      // start listening for incoming notifications
      self.client->LSP.LanguageClient.onNotification(self.id, json => {
        notificationChan->Chan.emit(json)
      })
      self
    })
  }

  let getHandle = conn => conn.handle
}

include Module
