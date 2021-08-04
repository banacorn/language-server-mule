open Belt
module Https = {
  @module("https")
  external get: (
    {"host": string, "path": string, "headers": {"User-Agent": string}},
    NodeJs.Http.IncomingMessage.t => unit,
  ) => unit = "get"

  @module("https")
  external getWithUrl: (string, NodeJs.Http.IncomingMessage.t => unit) => unit = "get"
}

module Error = {
  type t =
    | ServerResponseError(Js.Exn.t)
    | NoRedirectLocation
    | JsonParseError(string)
    | CannotWriteFile(Js.Exn.t)

  let toString = x =>
    switch x {
    | NoRedirectLocation => "Got HTTP 301/302 from GitHub without location in headers"
    | ServerResponseError(exn) => "Server response error:\n" ++ Util.JsError.toString(exn)
    | JsonParseError(raw) => "Cannot parse downloaded file as JSON:\n" ++ raw
    | CannotWriteFile(exn) =>
      "Failed to write downloaded content to files:\n" ++ Util.JsError.toString(exn)
    }
}

module Event = {
  type t =
    | Start
    | Progress(int, int)
    | Finish

  let toString = event =>
    switch event {
    | Start => "Start downloading"
    | Progress(accum, total) =>
      // if the file is larger than 10MB than we use MB as the unit
      total > 10485760
        ? "Downloading ( " ++
          string_of_int(accum / 1048576) ++
          " MB / " ++
          string_of_int(total / 1048576) ++ " MB )"
        : "Downloading ( " ++
          string_of_int(accum / 1024) ++
          " KB / " ++
          string_of_int(total / 1024) ++ " MB )"
    | Finish => "Finish downloading"
    }
}

module Module: {
  let asJson: {"headers": {"User-Agent": string}, "host": string, "path": string} => Promise.t<
    result<Js.Json.t, Error.t>,
  >
  let asFile: (
    {"headers": {"User-Agent": string}, "host": string, "path": string},
    string,
    Event.t => unit,
  ) => Promise.t<result<unit, Error.t>>
} = {
  let gatherDataFromResponse = res => {
    open NodeJs.Http.IncomingMessage
    let (promise, resolve) = Promise.pending()
    let body = ref("")
    res->onData(buf => body := body.contents ++ NodeJs.Buffer.toString(buf))->ignore
    res->onError(error => resolve(Error(Error.ServerResponseError(error))))->ignore
    res->onClose(() => resolve(Ok(body.contents)))->ignore
    promise
  }

  // with HTTP 301/302 redirect methodd
  let getWithRedirects = options => {
    let (promise, resolve) = Promise.pending()

    Https.get(options, res => {
      // check the response status code first
      let statusCode = NodeJs.Http.IncomingMessage.statusCode(res)
      switch statusCode {
      // redirect
      | 301
      | 302 =>
        let headers = NodeJs.Http.IncomingMessage.headers(res)
        switch headers.location {
        | None => resolve(Error(Error.NoRedirectLocation))
        | Some(urlAfterRedirect) =>
          Https.getWithUrl(urlAfterRedirect, resAfterRedirect => resolve(Ok(resAfterRedirect)))
        }
      // ok ?
      | _ => resolve(Ok(res))
      }
    })
    promise
  }

  let asJson = httpOptions =>
    getWithRedirects(httpOptions)
    ->Promise.flatMapOk(gatherDataFromResponse)
    ->Promise.flatMapOk(raw =>
      try {
        Promise.resolved(Ok(Js.Json.parseExn(raw)))
      } catch {
      | _ => Promise.resolved(Error(Error.JsonParseError(raw)))
      }
    )

  let asFile = (httpOptions, destPath, onDownload) =>
    getWithRedirects(httpOptions)->Promise.flatMapOk(res => {
      onDownload(Event.Start)
      // calculate and report download progress
      let totalSize =
        NodeJs.Http.IncomingMessage.headers(res).contentLenth->Option.mapWithDefault(
          0,
          int_of_string,
        )
      let accumSize = ref(0)
      res
      ->NodeJs.Http.IncomingMessage.onData(chunk => {
        let chunkSize = NodeJs.Buffer.length(chunk)
        accumSize := accumSize.contents + chunkSize
        onDownload(Event.Progress(accumSize.contents, totalSize))
      })
      ->ignore

      // pipe the response to a file
      let (promise, resolve) = Promise.pending()
      let fileStream = NodeJs.Fs.createWriteStream(destPath)
      fileStream
      ->NodeJs.Fs.WriteStream.onError(exn => resolve(Error(Error.CannotWriteFile(exn))))
      ->ignore
      fileStream
      ->NodeJs.Fs.WriteStream.onClose(() => {
        // report Event.Finish
        onDownload(Finish)
        // resolve the promise
        resolve(Ok())
      })
      ->ignore
      res->NodeJs.Http.IncomingMessage.pipe(fileStream)->ignore

      promise
    })
}
include Module
