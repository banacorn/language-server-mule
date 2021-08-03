module Unzip = Source__GitHub__Unzip
module Download = Source__GitHub__Download

module Nd = {
  module Fs = {
    @module("fs")
    external mkdirSync: string => unit = "mkdirSync"

    @module("fs")
    external unlink_raw: (string, Js.null<Js.Exn.t> => unit) => unit = "unlink"
    let unlink = path => {
      let (promise, resolve) = Promise.pending()
      unlink_raw(path, error => {
        switch Js.nullToOption(error) {
        | None => resolve(Ok())
        | Some(error) => resolve(Error(error))
        }
      })
      promise
    }

    @module("fs")
    external rename_raw: (string, string, Js.null<Js.Exn.t> => unit) => unit = "rename"
    let rename = (old, new) => {
      let (promise, resolve) = Promise.pending()
      rename_raw(old, new, error => {
        switch Js.nullToOption(error) {
        | None => resolve(Ok())
        | Some(error) => resolve(Error(error))
        }
      })
      promise
    }

    @module("fs")
    external readFile_raw: (string, (Js.null<Js.Exn.t>, NodeJs.Buffer.t) => unit) => unit =
      "readFile"
    let readFile = path => {
      let (promise, resolve) = Promise.pending()
      readFile_raw(path, (error, buffer) => {
        switch Js.nullToOption(error) {
        | None => resolve(Ok(buffer))
        | Some(error) => resolve(Error(error))
        }
      })
      promise
    }

    @module("fs")
    external createWriteStream: string => NodeJs.Fs.WriteStream.t = "createWriteStream"

    @module("fs")
    external createWriteStreamWithOptions: (string, {"mode": int}) => NodeJs.Fs.WriteStream.t =
      "createWriteStream"
  }

  module Url = {
    @module("url")
    external parse: string => {"host": string, "path": string, "protocol": string, "port": int} =
      "parse"
  }
}

module Error = {
  type t =
    // parsing
    | ResponseDecodeError(string, Js.Json.t)
    | NoMatchingRelease
    // download
    | AlreadyDownloading
    | CannotDownload(Download.Error.t)
    // file system
    | CannotDeleteFile(Js.Exn.t)
    | CannotRenameFile(Js.Exn.t)
    | CannotUnzipFile(Unzip.Error.t)

  let toString = x =>
    switch x {
    | AlreadyDownloading => "Already downloading"
    | ResponseDecodeError(msg, _) => "Cannot decode release metadata JSON from GitHub:\n" ++ msg
    // network
    | CannotDownload(error) => Download.Error.toString(error)
    // metadata
    | NoMatchingRelease => "Cannot matching release from GitHub"
    // file system
    | CannotDeleteFile(exn) => "Failed to delete files:\n" ++ Util.JsError.toString(exn)
    | CannotRenameFile(exn) => "Failed to rename files:\n" ++ Util.JsError.toString(exn)
    | CannotUnzipFile(error) => Unzip.Error.toString(error)
    }
}

module Asset = {
  type t = {
    name: string,
    url: string,
  }

  let decode = json => {
    open Json.Decode
    {
      url: json |> field("browser_download_url", string),
      name: json |> field("name", string),
    }
  }
}

module Release = {
  type t = {
    tagName: string,
    assets: array<Asset.t>,
  }

  let decode = json => {
    open Json.Decode
    {
      tagName: json |> field("tag_name", string),
      assets: json |> field("assets", array(Asset.decode)),
    }
  }

  let parseReleases = json =>
    try {
      Ok(json |> Json.Decode.array(decode))
    } catch {
    | Json.Decode.DecodeError(e) => Error(Error.ResponseDecodeError(e, json))
    }

  // NOTE: no caching
  let getReleasesFromGitHub = (username, repository, userAgent) => {
    let httpOptions = {
      "host": "api.github.com",
      "path": "/repos/" ++ username ++ "/" ++ repository ++ "/releases",
      "headers": {
        "User-Agent": userAgent,
      },
    }

    Download.asJson(httpOptions)->Promise.map(result =>
      switch result {
      | Error(e) => Error(Error.CannotDownload(e))
      | Ok(json) => parseReleases(json)
      }
    )
  }
}

module Target = {
  type t = {
    release: Release.t,
    asset: Asset.t,
    srcUrl: string,
    fileName: string,
  }
}

open Belt 

module Module: {
  type t = {
    username: string,
    repository: string,
    userAgent: string,
    globalStoragePath: string,
    chooseFromReleases: array<Release.t> => option<Target.t>,
  }
  let get: t => Promise.t<result<(string, Target.t), Error.t>>
} = {
  type t = {
    username: string,
    repository: string,
    userAgent: string,
    globalStoragePath: string,
    chooseFromReleases: array<Release.t> => option<Target.t>,
  }

  let inFlightDownloadFileName = "in-flight.download"

  // in-flight download will be named as "in-flight.download"
  // see if "in-flight.download" already exists
  let isDownloading = self => {
    if Node.Fs.existsSync(self.globalStoragePath) {
      let inFlightDownloadPath = NodeJs.Path.join2(self.globalStoragePath, inFlightDownloadFileName)
      let fileNames = NodeJs.Fs.readdirSync(self.globalStoragePath)
      let matched = fileNames->Array.keep(fileName => fileName == inFlightDownloadPath)
      matched[0]->Option.isSome
    } else {
      // create a directory for `context.globalStoragePath` if it doesn't exist
      Nd.Fs.mkdirSync(self.globalStoragePath)
      false
    }
  }

  let downloadLanguageServer = (self, target: Target.t) => {
    let url = Nd.Url.parse(target.srcUrl)
    let httpOptions = {
      "host": url["host"],
      "path": url["path"],
      "headers": {
        "User-Agent": "gcl-vscode",
      },
    }

    let inFlightDownloadPath = NodeJs.Path.join2(self.globalStoragePath, inFlightDownloadFileName)
    let destPath = Node_path.join2(self.globalStoragePath, target.fileName)

    Download.asFile(httpOptions, inFlightDownloadPath)
    ->Promise.mapError(e => Error.CannotDownload(e))
    // suffix with ".zip" after downloaded
    ->Promise.flatMapOk(() =>
      Nd.Fs.rename(
        inFlightDownloadPath,
        inFlightDownloadPath ++ ".zip",
      )->Promise.mapError(e => Error.CannotRenameFile(e))
    )
    // unzip the downloaded file
    ->Promise.flatMapOk(() => {
      Unzip.run(inFlightDownloadPath ++ ".zip", destPath)->Promise.mapError(error => Error.CannotUnzipFile(
        error,
      ))
    })
    // remove the zip file
    ->Promise.flatMapOk(() =>
      Nd.Fs.unlink(inFlightDownloadPath ++ ".zip")->Promise.mapError(e => Error.CannotDeleteFile(e))
    )
    // cleanup on error
    ->Promise.flatMap(result =>
      switch result {
      | Error(error) =>
        let remove = path => {
          if Node.Fs.existsSync(path) {
            Nd.Fs.unlink(path)->Promise.map(_ => ())
          } else {
            Promise.resolved()
          }
        }
        Promise.allArray([
          remove(inFlightDownloadPath),
          remove(inFlightDownloadPath ++ ".zip"),
        ])->Promise.map(_ => Error(error))
      | Ok() => Promise.resolved(Ok((destPath, target)))
      }
    )
  }

  let get = self => {
    if isDownloading(self) {
      Promise.resolved(Error(Error.AlreadyDownloading))
    } else {
      Release.getReleasesFromGitHub(self.username, self.repository, self.userAgent)
      ->Promise.mapOk(self.chooseFromReleases)
      ->Promise.flatMapOk(result =>
        switch result {
        | None => Promise.resolved(Error(Error.NoMatchingRelease))
        | Some(target) => 
            // don't download from GitHub if `target.fileName` already exists 
            let destPath = NodeJs.Path.join2(self.globalStoragePath, target.fileName)
            if NodeJs.Fs.existsSync(destPath) {
              Js.log("[ mule ] Used cached program")
              Promise.resolved(Ok((destPath, target )))
            } else {
              Js.log("[ mule ] Download from GitHub")
              downloadLanguageServer(self, target)
            }
        
        }
      )
    }
  }
}

include Module
