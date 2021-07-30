// module Error = {
//   type t =
//     // connection
//     // | ConnectionError(Js.Exn.t)
//     | CannotSendRequest(Js.Exn.t)
//     | CannotSendNotification(Js.Exn.t)

//   let toString = x =>
//     switch x {
//     // | ConnectionError(exn) => "Connection error: " ++ Util.JsError.toString(exn)
//     | CannotSendRequest(exn) => "Cannot send request: " ++ Util.JsError.toString(exn)
//     | CannotSendNotification(exn) => "Cannot send notification: " ++ Util.JsError.toString(exn)
//     }
// }

module LSP = Client__LSP__Binding

module type Module = {
  type t
  // lifecycle
  let make: (string, string, Source.t, Method.t) => Promise.t<result<t, Js.Exn.t>>
  let destroy: t => Promise.t<unit>
  // request / notification / error
  let sendRequest: (t, Js.Json.t) => Promise.t<result<Js.Json.t, Js.Exn.t>>
  let onRequest: (t, Js.Json.t => Promise.t<result<Js.Json.t, Js.Exn.t>>) => unit
  let sendNotification: (t, Js.Json.t) => Promise.promise<result<unit, Js.Exn.t>>
  let onNotification: (t, Js.Json.t => unit) => VSCode.Disposable.t
  let onError: (t, Js.Exn.t => unit) => VSCode.Disposable.t
  // channels for notification & error
  let getNotificationChan: t => Chan.t<Js.Json.t>
  let getErrorChan: t => Chan.t<Js.Exn.t>
  // properties
  let getSourceAndMethod: t => (Source.t, Method.t)
}

module Module: Module = {
  open VSCode

  type t = {
    client: LSP.LanguageClient.t,
    id: string, // language id, also for identifying custom methods
    name: string, // name for the language server client
    sourceAndMethod: (Source.t, Method.t),
    // event emitters
    errorChan: Chan.t<Js.Exn.t>,
    notificationChan: Chan.t<Js.Json.t>,
    // method of the client itself
    subscription: VSCode.Disposable.t,
  }

  let onError = (self, callback) =>
    self.errorChan->Chan.on(e => callback(e))->VSCode.Disposable.make
  let getErrorChan = self => self.errorChan

  let onNotification = (self, callback) =>
    self.notificationChan->Chan.on(callback)->VSCode.Disposable.make
  let getNotificationChan = self => self.notificationChan
  let sendNotification = (self, data) =>
    self.client
    ->LSP.LanguageClient.onReady
    ->Promise.Js.toResult
    ->Promise.mapOk(() => {
      self.client->LSP.LanguageClient.sendNotification(self.id, data)
    })

  let sendRequest = (self, data) =>
    self.client
    ->LSP.LanguageClient.onReady
    ->Promise.Js.toResult
    ->Promise.flatMapOk(() => {
      self.client->LSP.LanguageClient.sendRequest(self.id, data)->Promise.Js.toResult
    })

  let onRequest = (self, callback) => self.client->LSP.LanguageClient.onRequest(self.id, callback)

  let destroy = self => {
    self.errorChan->Chan.destroy
    self.notificationChan->Chan.destroy
    self.subscription->VSCode.Disposable.dispose->ignore
    self.client->LSP.LanguageClient.stop->Promise.Js.toResult->Promise.map(_ => ())
  }

  let make = (id, name, source, method) => {
    let errorChan = Chan.make()

    let serverOptions = switch method {
    | Method.ViaTCP(port, _host) => LSP.ServerOptions.makeWithStreamInfo(port, "localhost")
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
    let languageClient = LSP.LanguageClient.make(id, name, serverOptions, clientOptions)

    let self = {
      client: languageClient,
      id: id,
      name: name,
      sourceAndMethod: (source, method),
      errorChan: errorChan,
      notificationChan: Chan.make(),
      subscription: languageClient->LSP.LanguageClient.start,
    }

    // Let `LanguageClient.onReady` and `errorChan->Chan.once` race
    Promise.race(list{
      self.client->LSP.LanguageClient.onReady->Promise.Js.toResult,
      errorChan->Chan.once->Promise.map(err => Error(err)),
    })->Promise.mapOk(() => {
      // start listening for incoming notifications
      self.client->LSP.LanguageClient.onNotification(self.id, json => {
        self.notificationChan->Chan.emit(json)
      })
      self
    })
  }

  let getSourceAndMethod = conn => conn.sourceAndMethod
}

include Module
