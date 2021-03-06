module LSP = Client__LSP__Binding

module type Module = {
  type t
  // lifecycle
  let make: (string, string, Method.t, Js.Json.t) => Promise.t<result<t, Js.Exn.t>>
  let destroy: t => Promise.t<unit>
  // request / notification / error
  let sendRequest: (t, Js.Json.t) => Promise.t<result<Js.Json.t, Js.Exn.t>>
  let onRequest: (t, Js.Json.t => Promise.t<result<Js.Json.t, Js.Exn.t>>) => VSCode.Disposable.t
  let sendNotification: (t, Js.Json.t) => Promise.promise<result<unit, Js.Exn.t>>
  let onNotification: (t, Js.Json.t => unit) => VSCode.Disposable.t
  let onError: (t, Js.Exn.t => unit) => VSCode.Disposable.t
  // channels for notification & error
  let getNotificationChan: t => Chan.t<Js.Json.t>
  let getErrorChan: t => Chan.t<Js.Exn.t>
  // properties
  let getMethod: t => Method.t
}

module Module: Module = {
  open VSCode

  type t = {
    client: LSP.LanguageClient.t,
    id: string, // language id, also for identifying custom methods
    name: string, // name for the language server client
    method: Method.t,
    // event emitters
    errorChan: Chan.t<Js.Exn.t>,
    notificationChan: Chan.t<Js.Json.t>,
    // handles of listeners
    subscriptions: array<VSCode.Disposable.t>,
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
    ->Promise.flatMapOk(() => {
      self.client->LSP.LanguageClient.sendNotification(self.id, data)->Promise.Js.toResult
    })

  let sendRequest = (self, data) =>
    self.client
    ->LSP.LanguageClient.onReady
    ->Promise.Js.toResult
    ->Promise.flatMapOk(() => {
      self.client->LSP.LanguageClient.sendRequest(self.id, data)->Promise.Js.toResult
    })

  let onRequest = (self, callback) =>
    self.client->LSP.LanguageClient.onRequest(self.id, callback)->LSP.Disposable.toVSCodeDisposable

  let destroy = self => {
    self.errorChan->Chan.destroy
    self.notificationChan->Chan.destroy
    self.subscriptions->Belt.Array.forEach(VSCode.Disposable.dispose)->ignore
    LSP.LanguageClient.stop(self.client)
  }

  let make = (id, name, method, initializationOptions) => {
    let errorChan = Chan.make()

    let serverOptions = switch method {
    | Method.ViaTCP(port, host, _) => LSP.ServerOptions.makeWithStreamInfo(port, host)
    | ViaCommand(path, args, options, _) => LSP.ServerOptions.makeWithCommand(path, args, options)
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

      LSP.LanguageClientOptions.make(
        documentSelector,
        synchronize,
        errorHandler,
        initializationOptions,
      )
    }

    // Create the language client
    let languageClient = LSP.LanguageClient.make(id, name, serverOptions, clientOptions)

    let self = {
      client: languageClient,
      id: id,
      name: name,
      method: method,
      errorChan: errorChan,
      notificationChan: Chan.make(),
      subscriptions: [languageClient->LSP.LanguageClient.start->LSP.Disposable.toVSCodeDisposable],
    }

    // Let `LanguageClient.onReady` and `errorChan->Chan.once` race
    Promise.race(list{
      self.client->LSP.LanguageClient.onReady->Promise.Js.toResult,
      errorChan->Chan.once->Promise.map(err => Error(err)),
    })->Promise.mapOk(() => {
      // start listening for incoming notifications
      self.client
      ->LSP.LanguageClient.onNotification(self.id, json => {
        self.notificationChan->Chan.emit(json)
      })
      ->LSP.Disposable.toVSCodeDisposable
      ->Js.Array.push(self.subscriptions)
      ->ignore
      self
    })
  }

  let getMethod = conn => conn.method
}

include Module
