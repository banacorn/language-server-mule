module Error = {
  type t =
    // connection
    | ConnectionError(Js.Exn.t)
    | CannotSendRequest(Js.Exn.t)
    | CannotSendNotification(Js.Exn.t)

  let toString = x => switch x {
  | ConnectionError(exn) => "Connection error: " ++ Util.JsError.toString(exn)
  | CannotSendRequest(exn) => "Cannot send request: " ++ Util.JsError.toString(exn)
  | CannotSendNotification(exn) => "Cannot send notification: " ++ Util.JsError.toString(exn)
  }
}
module LSP = Client__LSP__Binding

module type Module = {
  type t
  // lifecycle
  let make: (string, string, Handle.t) => Promise.t<result<t, Error.t>>
  let destroy: t => Promise.t<unit>
  // request / notification / error
  let sendRequest: (t, Js.Json.t) => Promise.t<result<Js.Json.t, Error.t>>
  let onRequest: (t, Js.Json.t => unit) => VSCode.Disposable.t
  let sendNotification: (t, Js.Json.t) => Promise.promise<result<unit, Error.t>>
  let onNotification: (t, Js.Json.t => unit) => VSCode.Disposable.t
  let onError: (t, Error.t => unit) => VSCode.Disposable.t
  // properties
  let getHandle: t => Handle.t
}

module Module: Module = {
  open VSCode

  type t = {
    client: LSP.LanguageClient.t,
    id: string, // language id, also for identifying custom methods
    name: string, // name for the language server client 
    handle: Handle.t,
    // event emitters
    errorChan: Chan.t<Js.Exn.t>,
    requestChan: Chan.t<Js.Json.t>,
    notificationChan: Chan.t<Js.Json.t>,
    // handle of the client itself
    subscription: VSCode.Disposable.t,
  }

  let onError = self => callback =>
    self.errorChan->Chan.on(e => callback(Error.ConnectionError(e)))->VSCode.Disposable.make
  let onNotification = self => callback => self.notificationChan->Chan.on(callback)->VSCode.Disposable.make
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

  let onRequest = self => callback => self.requestChan->Chan.on(callback)->VSCode.Disposable.make

  let destroy = self => {
    self.errorChan->Chan.destroy
    self.notificationChan->Chan.destroy
    self.requestChan->Chan.destroy
    self.subscription->VSCode.Disposable.dispose->ignore
    self.client->LSP.LanguageClient.stop->Promise.Js.toResult->Promise.map(_ => ())
  }

  let make = (id, name, handle) => {

    let errorChan = Chan.make()

    let serverOptions = switch handle {
    | Handle.ViaTCP(port, _host) => LSP.ServerOptions.makeWithStreamInfo(port, "localhost")
    | ViaStdIO(path) => LSP.ServerOptions.makeWithCommand(path)
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
      id: id,
      name: name,
      handle: handle,
      errorChan: errorChan,
      notificationChan: Chan.make(),
      requestChan: Chan.make(),
      subscription: languageClient->LSP.LanguageClient.start,
    }

    // Let `LanguageClient.onReady` and `errorChan->Chan.once` race
    Promise.race(list{
      self.client->LSP.LanguageClient.onReady->Promise.Js.toResult,
      errorChan->Chan.once->Promise.map(err => Error(err)),
    })
    ->Promise.mapError(error => Error.ConnectionError(error))
    ->Promise.mapOk(() => {
      // start listening for incoming requests notifications
      self.client->LSP.LanguageClient.onRequest(self.id, json => {
        self.requestChan->Chan.emit(json)
        Promise.resolved()
      })
      // start listening for incoming notifications
      self.client->LSP.LanguageClient.onNotification(self.id, json => {
        self.notificationChan->Chan.emit(json)
      })
      self
    })
  }

  let getHandle = conn => conn.handle
}

include Module
