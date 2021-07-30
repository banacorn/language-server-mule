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
    | CannotWriteFile(exn) => "Failed to write downloaded content to files:\n" ++ Util.JsError.toString(exn)
    }
}

module Module: {
  let asJson: {"headers": {"User-Agent": string}, "host": string, "path": string} => Promise.t<
    result<Js.Json.t, Error.t>,
  >
  let asFile: (
    {"headers": {"User-Agent": string}, "host": string, "path": string},
    string,
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

  let asFile = (httpOptions, destPath) =>
    getWithRedirects(httpOptions)->Promise.flatMapOk(res => {
      let (promise, resolve) = Promise.pending()
      let fileStream = NodeJs.Fs.createWriteStream(destPath)
      fileStream
      ->NodeJs.Fs.WriteStream.onError(exn => resolve(Error(Error.CannotWriteFile(exn))))
      ->ignore
      fileStream->NodeJs.Fs.WriteStream.onClose(() => resolve(Ok()))->ignore
      res->NodeJs.Http.IncomingMessage.pipe(fileStream)->ignore
      promise
    })
}
include Module
