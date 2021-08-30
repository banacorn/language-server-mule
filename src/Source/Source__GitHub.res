module Unzip = Source__GitHub__Unzip
module Unzip2 = Source__GitHub__Unzip2
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
    external writeFile_raw: (string, NodeJs.Buffer.t, Js.null<Js.Exn.t> => unit) => unit =
      "writeFile"
    let writeFile = (path, data) => {
      let (promise, resolve) = Promise.pending()
      writeFile_raw(path, data, error => {
        switch Js.nullToOption(error) {
        | None => resolve(Ok())
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
    | ResponseDecodeError(string, Js.Json.t)
    | JsonParseError(string)
    | NoMatchingRelease
    // download
    | AlreadyDownloading
    | CannotDownload(Download.Error.t)
    // cacheing
    | CannotCacheReleases(Js.Exn.t)
    // file system
    | CannotStatFile(string)
    | CannotChmodFile(string)
    | CannotReadFile(Js.Exn.t)
    | CannotDeleteFile(Js.Exn.t)
    | CannotRenameFile(Js.Exn.t)
    | CannotUnzipFile(Unzip.Error.t)

  let toString = x =>
    switch x {
    | ResponseDecodeError(msg, _) => "Cannot decode release metadata JSON from GitHub:\n" ++ msg
    | JsonParseError(raw) => "Cannot parse string as JSON:\n" ++ raw
    | NoMatchingRelease => "Cannot find matching release from GitHub"
    // download
    | CannotDownload(error) => Download.Error.toString(error)
    | AlreadyDownloading => "Already downloading"
    // cacheing
    | CannotCacheReleases(exn) => "Failed to cache releases:\n" ++ Util.JsError.toString(exn)
    // file system
    | CannotStatFile(path) => "Cannot stat file \"" ++ path ++ "\""
    | CannotChmodFile(path) => "Cannot chmod file \"" ++ path ++ "\" to make it executable"
    | CannotReadFile(exn) => "Cannot to read files:\n" ++ Util.JsError.toString(exn)
    | CannotDeleteFile(exn) => "Cannot to delete files:\n" ++ Util.JsError.toString(exn)
    | CannotRenameFile(exn) => "Cannot to rename files:\n" ++ Util.JsError.toString(exn)
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

  let encode = asset => {
    open Json.Encode
    object_(list{("browser_download_url", asset.url |> string), ("name", asset.name |> string)})
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

  let encode = release => {
    open Json.Encode
    object_(list{
      ("tag_name", release.tagName |> string),
      ("assets", release.assets |> array(Asset.encode)),
    })
  }

  let parseReleases = json =>
    try {
      Ok(json |> Json.Decode.array(decode))
    } catch {
    | Json.Decode.DecodeError(e) => Error(Error.ResponseDecodeError(e, json))
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
    onDownload: Download.Event.t => unit,
    log: string => unit,
    cacheInvalidateExpirationSecs: int,
    cacheID: string,
  }
  let get: t => Promise.t<result<(string, Target.t), Error.t>>
  let chmodExecutable: string => Promise.t<result<unit, Error.t>>
  let log: string => unit
} = {
  type t = {
    username: string,
    repository: string,
    userAgent: string,
    globalStoragePath: string,
    chooseFromReleases: array<Release.t> => option<Target.t>,
    onDownload: Download.Event.t => unit,
    log: string => unit,
    cacheInvalidateExpirationSecs: int,
    cacheID: string,
  }

  // helper function for chmoding 744 the executable
  let chmodExecutable = path =>
    NodeJs.Fs.chmod(path, ~mode=0o744)
    ->Promise.Js.fromBsPromise
    ->Promise.Js.toResult
    ->Promise.mapError(_ => Error.CannotStatFile(path))

  let log = Js.log

  let getAssetPath = (self, target: Target.t) =>
    NodeJs.Path.join2(self.globalStoragePath, target.fileName)
  let getFlightDownloadPath = self =>
    NodeJs.Path.join2(self.globalStoragePath, "in-flight.download")

  // see if "in-flight.download" already exists
  let isDownloading = self => {
    if Node.Fs.existsSync(self.globalStoragePath) {
      let fileNames = NodeJs.Fs.readdirSync(self.globalStoragePath)
      let matched = fileNames->Array.keep(fileName => fileName == getFlightDownloadPath(self))
      matched[0]->Option.isSome
    } else {
      // create a directory for `context.globalStoragePath` if it doesn't exist
      Nd.Fs.mkdirSync(self.globalStoragePath)
      false
    }
  }

  let unzip = (self, target) => {
    let assetPath = getAssetPath(self, target)
    let zipPath = assetPath ++ ".zip"

    self.log("[ unzip ] Unzipping \"" ++ assetPath ++ "\"")
    // suffix the downloaded file with ".zip"
    Nd.Fs.rename(assetPath, zipPath)
    ->Promise.mapError(e => Error.CannotRenameFile(e))
    // unzip
    ->Promise.flatMapOk(() => Unzip2.run(zipPath, assetPath)->Promise.map(() => Ok()))
    // remove the zip file
    ->Promise.flatMapOk(() =>
      Nd.Fs.unlink(zipPath)->Promise.mapError(e => Error.CannotDeleteFile(e))
    )
    ->Promise.mapOk(() => target)
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

    Download.asFile(httpOptions, getFlightDownloadPath(self), self.onDownload)
    ->Promise.mapError(e => Error.CannotDownload(e))
    // cleanup on error
    ->Promise.flatMapError(error => {
      let remove = path => {
        if Node.Fs.existsSync(path) {
          Nd.Fs.unlink(path)->Promise.map(_ => ())
        } else {
          Promise.resolved()
        }
      }
      remove(getFlightDownloadPath(self))->Promise.map(_ => Error(error))
    })
    // rename on success
    ->Promise.flatMapOk(() =>
      Nd.Fs.rename(
        getFlightDownloadPath(self),
        getAssetPath(self, target),
      )->Promise.mapError(error => Error.CannotRenameFile(error))
    )
    ->Promise.mapOk(() => target)
  }

  // NOTE: no caching
  // timeouts after 1000ms
  let getReleasesFromGitHub = self => {
    let httpOptions = {
      "host": "api.github.com",
      "path": "/repos/" ++ self.username ++ "/" ++ self.repository ++ "/releases",
      "headers": {
        "User-Agent": self.userAgent,
      },
    }

    Download.asJson(httpOptions)
    ->Download.timeoutAfter(1000)
    ->Promise.map(result =>
      switch result {
      | Error(e) => Error(Error.CannotDownload(e))
      | Ok(json) => Release.parseReleases(json)
      }
    )
  }

  module Cache = {
    // util for getting stat modify time in ms
    let statModifyTime = path =>
      NodeJs.Fs.lstat(path)
      ->Promise.Js.fromBsPromise
      ->Promise.Js.toResult
      ->Promise.mapError(_ => Error.CannotStatFile(path))
      ->Promise.mapOk(stat => stat.mtimeMs)

    let cachePath = self =>
      NodeJs.Path.join2(self.globalStoragePath, "releases-cache-" ++ self.cacheID ++ ".json")

    let isValid = self => {
      let path = cachePath(self)

      if NodeJs.Fs.existsSync(path) {
        statModifyTime(path)->Promise.map(result =>
          switch result {
          | Error(_) => false // invalidate when there's an error
          | Ok(lastModifiedTime) =>
            let currentTime = Js.Date.now()
            // devise time difference in seconds
            let diff = int_of_float((currentTime -. lastModifiedTime) /. 1000.0)
            // cache is invalid if it is too old
            diff < self.cacheInvalidateExpirationSecs
          }
        )
      } else {
        // the cache does not exist, hence not valid
        Promise.resolved(false)
      }
    }

    let persist = (self, releases) => {
      let json =
        Json_encode.array(Release.encode, releases)->Js_json.stringify->NodeJs.Buffer.fromString
      let path = cachePath(self)
      Nd.Fs.writeFile(path, json)->Promise.map(result =>
        switch result {
        | Error(e) => Error(Error.CannotCacheReleases(e))
        | Ok() => Ok(releases) // pass it on for chaining
        }
      )
    }
  }

  // use cached releases instead of fetching them from GitHub, if the cached releases data is not too old (24 hrs)
  let getReleases = self => {
    let path = Cache.cachePath(self)

    Cache.isValid(self)->Promise.flatMap(isValid =>
      if isValid {
        self.log("Use cached releases data")
        // use the cached releases data
        Nd.Fs.readFile(path)
        ->Promise.mapError(e => Error.CannotRenameFile(e))
        // decode file as json
        ->Promise.flatMapOk(buffer => {
          let string = NodeJs.Buffer.toString(buffer)
          try {
            Promise.resolved(Ok(Js.Json.parseExn(string)))
          } catch {
          | _ => Promise.resolved(Error(Error.JsonParseError(string)))
          }
        })
        // parse the json
        ->Promise.flatMapOk(json => {
          switch Release.parseReleases(json) {
          | Error(e) => Promise.resolved(Error(e))
          | Ok(releases) => Promise.resolved(Ok(releases))
          }
        })
      } else {
        self.log("GitHub releases cache invalidated")
        getReleasesFromGitHub(self)->Promise.flatMapOk(Cache.persist(self))
      }
    )
  }

  let get = self => {
    if isDownloading(self) {
      Promise.resolved(Error(Error.AlreadyDownloading))
    } else {
      getReleases(self)
      ->Promise.mapOk(self.chooseFromReleases)
      ->Promise.flatMapOk(result =>
        switch result {
        | None => Promise.resolved(Error(Error.NoMatchingRelease))
        | Some(target) =>
          let assetPath = getAssetPath(self, target)
          // don't download from GitHub if `target.fileName` already exists
          if NodeJs.Fs.existsSync(assetPath) {
            self.log("Used downloaded program")
            Promise.resolved(Ok(assetPath, target))
          } else {
            self.log("Download from GitHub instead")
            downloadLanguageServer(self, target)
            ->Promise.flatMapOk(unzip(self))
            ->Promise.mapOk(_ => (assetPath, target))
          }
        }
      )
    }
  }
}

include Module
