// module for unzipping downloaded files
module Yauzl = {
  module Entry = {
    type t
  }
  module ZipFile = {
    type t
    @send
    external openReadStream: (
      t,
      Entry.t,
      (Js.null<Js.Exn.t>, option<NodeJs.Stream.Readable.t<NodeJs.Buffer.t>>) => unit,
    ) => unit = "openReadStream"

    @send
    external onEntry: (t, @as("entry") _, Entry.t => unit) => unit = "on"
  }

  @module("yauzl")
  external open_: (string, (Js.null<Js.Exn.t>, option<ZipFile.t>) => unit) => unit = "open"
}

module Error = {
  type t =
    | CannotUnzip(string, Js.Exn.t)
    | FileIsEmpty(string)

  let toString = x =>
    switch x {
    | CannotUnzip(filepath, exn) =>
      "Cannot unzip \"" ++ filepath ++ "\": " ++ Util.JsError.toString(exn)
    | FileIsEmpty(filepath) => "Cannot unzip \"" ++ filepath ++ "\" because it's empty"
    }
}

let run = (src, dest) => {
  let (promise, resolve) = Promise.pending()

  // chmod 744 the executable
  let fileWriteStream = NodeJs.Fs.createWriteStreamWith(
    dest,
    NodeJs.Fs.createWriteStreamOptions(~mode=0o744, ()),
  )

  // success on `close`, failure on `error`
  fileWriteStream
  ->NodeJs.Fs.WriteStream.onErrorOnce(exn => resolve(Error(Error.CannotUnzip(src, exn))))
  ->NodeJs.Fs.WriteStream.onCloseOnce(() => {
    resolve(Ok())
  })
  ->ignore

  // start unzipping the file
  Yauzl.open_(src, (err, result) => {
    switch Js.nullToOption(err) {
    | Some(err) => resolve(Error(CannotUnzip(src, err)))
    | None =>
      switch result {
      | None => resolve(Error(FileIsEmpty(src)))
      | Some(zipFile) =>
        // We only expect *one* file inside each zip
        zipFile->Yauzl.ZipFile.onEntry(entry => {
          zipFile->Yauzl.ZipFile.openReadStream(entry, (err2, result2) => {
            switch Js.nullToOption(err2) {
            | Some(err2) => resolve(Error(CannotUnzip(src, err2)))
            | None =>
              switch result2 {
              | None => resolve(Error(FileIsEmpty(src)))
              | Some(readStream) => readStream->NodeJs.Stream.Readable.pipe(fileWriteStream)->ignore
              }
            }
          })
        })
      }
    }
  })
  promise
}
