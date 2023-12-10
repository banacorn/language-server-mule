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
    | CannotGetReleases(Download.Error.t)
    // cacheing
    | CannotCacheReleases(Js.Exn.t)
    // file system
    | CannotChmodFile(string)
    | CannotStatFile(string)
    | CannotReadFile(Js.Exn.t)
    | CannotDeleteFile(Js.Exn.t)
    | CannotRenameFile(Js.Exn.t)

  let toString = x =>
    switch x {
    | ResponseDecodeError(msg, _) => "Cannot decode release metadata JSON from GitHub:\n" ++ msg
    | JsonParseError(raw) => "Cannot parse string as JSON:\n" ++ raw
    | NoMatchingRelease => "Cannot find matching release from GitHub"
    // download
    | CannotDownload(error) =>
      "Cannot downlaod file from GitHub:\n" ++ Download.Error.toString(error)
    | CannotGetReleases(error) =>
      "Cannot get release info from GitHub:\n" ++ Download.Error.toString(error)
    | AlreadyDownloading => "Already downloading"
    // cacheing
    | CannotCacheReleases(exn) => "Failed to cache releases:\n" ++ Util.JsError.toString(exn)
    // file system
    | CannotStatFile(path) => "Cannot stat file \"" ++ path ++ "\""
    | CannotChmodFile(path) => "Cannot chmod file \"" ++ path ++ "\""
    | CannotReadFile(exn) => "Cannot to read files:\n" ++ Util.JsError.toString(exn)
    | CannotDeleteFile(exn) => "Cannot to delete files:\n" ++ Util.JsError.toString(exn)
    | CannotRenameFile(exn) => "Cannot to rename files:\n" ++ Util.JsError.toString(exn)
    }
}

module Asset = {
  type t = {
    url: string,
    id: int,
    node_id: string,
    name: string,
    label: string,
    content_type: string,
    state: string,
    size: int,
    created_at: string,
    updated_at: string,
    browser_download_url: string,
  }

  let decode = {
    open JsonCombinators.Json.Decode
    object(field => {
      url: field.required(. "url", string),
      id: field.required(. "id", int),
      node_id: field.required(. "node_id", string),
      name: field.required(. "name", string),
      label: field.required(. "label", string),
      content_type: field.required(. "content_type", string),
      state: field.required(. "state", string),
      size: field.required(. "size", int),
      created_at: field.required(. "created_at", string),
      updated_at: field.required(. "updated_at", string),
      browser_download_url: field.required(. "browser_download_url", string),
    })
  }

  let encode = asset => {
    open JsonCombinators.Json.Encode
    Unsafe.object({
      "url": string(asset.url),
      "id": int(asset.id),
      "node_id": string(asset.node_id),
      "name": string(asset.name),
      "label": string(asset.label),
      "content_type": string(asset.content_type),
      "state": string(asset.state),
      "size": int(asset.size),
      "created_at": string(asset.created_at),
      "updated_at": string(asset.updated_at),
      "browser_download_url": string(asset.browser_download_url),
    })
  }
}

module Release = {
  type t = {
    url: string,
    assets_url: string,
    upload_url: string,
    html_url: string,
    id: int,
    node_id: string,
    tag_name: string,
    target_commitish: string,
    name: string,
    draft: bool,
    prerelease: bool,
    created_at: string,
    published_at: string,
    assets: array<Asset.t>,
    tarball_url: string,
    zipball_url: string,
    body: option<string>,
  }

  let decode = {
    open JsonCombinators.Json.Decode
    object(field => {
      url: field.required(. "url", string),
      assets_url: field.required(. "assets_url", string),
      upload_url: field.required(. "upload_url", string),
      html_url: field.required(. "html_url", string),
      id: field.required(. "id", int),
      node_id: field.required(. "node_id", string),
      tag_name: field.required(. "tag_name", string),
      target_commitish: field.required(. "target_commitish", string),
      name: field.required(. "name", string),
      draft: field.required(. "draft", bool),
      prerelease: field.required(. "prerelease", bool),
      created_at: field.required(. "created_at", string),
      published_at: field.required(. "published_at", string),
      assets: field.required(. "assets", array(Asset.decode)),
      tarball_url: field.required(. "tarball_url", string),
      zipball_url: field.required(. "zipball_url", string),
      body: field.required(. "body", option(string)),
    })
  }

  let encode = release => {
    open JsonCombinators.Json.Encode
    Unsafe.object({
      "url": string(release.url),
      "assets_url": string(release.assets_url),
      "upload_url": string(release.upload_url),
      "html_url": string(release.html_url),
      "id": int(release.id),
      "node_id": string(release.node_id),
      "tag_name": string(release.tag_name),
      "target_commitish": string(release.target_commitish),
      "name": string(release.name),
      "draft": bool(release.draft),
      "prerelease": bool(release.prerelease),
      "created_at": string(release.created_at),
      "published_at": string(release.published_at),
      "assets": array(Asset.encode, release.assets),
      "tarball_url": string(release.tarball_url),
      "zipball_url": string(release.zipball_url),
      "body": option(string, release.body),
    })
  }

  let encodeReleases = releases => {
    open JsonCombinators.Json.Encode
    array(encode, releases)
  }

  let decodeReleases = json => {
    switch JsonCombinators.Json.decode(json, JsonCombinators.Json.Decode.array(decode)) {
    | Ok(releases) => Ok(releases)
    | Error(e) => Error(Error.ResponseDecodeError(e, json))
    }
  }
}

module Target = {
  type t = {
    release: Release.t,
    asset: Asset.t,
    saveAsFileName: string,
  }
}

open Belt

// helper function for chmoding 744 the executable
let chmodExecutable = path =>
  NodeJs.Fs.chmod(path, ~mode=0o744)
  ->Promise.Js.fromBsPromise
  ->Promise.Js.toResult
  ->Promise.mapError(_ => Error.CannotChmodFile(path))

module Module: {
  type t = {
    username: string,
    repository: string,
    userAgent: string,
    globalStoragePath: string,
    chooseFromReleases: array<Release.t> => option<Target.t>,
    onDownload: Download.Event.t => unit,
    afterDownload: (
      bool,
      (string, Target.t),
    ) => Promise.t<
      result<
        (string, array<string>, option<Client__LSP__Binding.ExecutableOptions.t>, Target.t),
        Error.t,
      >,
    >,
    cacheInvalidateExpirationSecs: int,
    log: string => unit,
  }
  // let get: t => Promise.t<result<(string, Target.t), Error.t>>
  let get: t => Promise.t<
    result<
      (string, array<string>, option<Client__LSP__Binding.ExecutableOptions.t>, Target.t),
      Error.t,
    >,
  >
} = {
  type t = {
    username: string,
    repository: string,
    userAgent: string,
    globalStoragePath: string,
    chooseFromReleases: array<Release.t> => option<Target.t>,
    onDownload: Download.Event.t => unit,
    afterDownload: (
      bool,
      (string, Target.t),
    ) => Promise.t<
      result<
        (string, array<string>, option<Client__LSP__Binding.ExecutableOptions.t>, Target.t),
        Error.t,
      >,
    >,
    cacheInvalidateExpirationSecs: int,
    log: string => unit,
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
    let url = Nd.Url.parse(target.asset.browser_download_url)
    let httpOptions = {
      "host": url["host"],
      "path": url["path"],
      "headers": {
        "User-Agent": self.userAgent,
      },
    }

    let inFlightDownloadPath = NodeJs.Path.join2(self.globalStoragePath, inFlightDownloadFileName)
    let destPath = Node_path.join2(self.globalStoragePath, target.saveAsFileName)

    Download.asFile(httpOptions, inFlightDownloadPath, self.onDownload)
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
      Unzip.run(inFlightDownloadPath ++ ".zip", destPath)->Promise.map(() => Ok())
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
    ->Download.timeoutAfter(10000)
    ->Promise.map(result =>
      switch result {
      | Error(e) => Error(Error.CannotGetReleases(e))
      | Ok(json) => Release.decodeReleases(json)
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

    let cachePath = self => NodeJs.Path.join2(self.globalStoragePath, "releases-cache.json")

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
      let json = Release.encodeReleases(releases)->Js_json.stringify->NodeJs.Buffer.fromString
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
        self.log("[ mule ] Use cached releases data at:" ++ path)
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
          switch Release.decodeReleases(json) {
          | Error(e) => Promise.resolved(Error(e))
          | Ok(releases) => Promise.resolved(Ok(releases))
          }
        })
      } else {
        self.log("[ mule ] GitHub releases cache invalidated")
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
          // don't download from GitHub if `target.fileName` already exists
          let destPath = NodeJs.Path.join2(self.globalStoragePath, target.saveAsFileName)
          if NodeJs.Fs.existsSync(destPath) {
            self.log("[ mule ] Used downloaded program at:" ++ destPath)
            self.afterDownload(true, (destPath, target))
          } else {
            self.log("[ mule ] Download from GitHub instead")
            downloadLanguageServer(self, target)->Promise.flatMapOk(self.afterDownload(false))
          }
        }
      )
    }
  }
}

include Module
