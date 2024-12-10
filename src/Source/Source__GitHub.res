module Unzip = Source__GitHub__Unzip
module Download = Source__GitHub__Download

module Nd = {
  module Fs = {
    @module("node:fs") @scope("promises")
    external readdir: string => promise<array<string>> = "readdir"

    @module("fs")
    external unlink_raw: (string, Js.null<Js.Exn.t> => unit) => unit = "unlink"
    let unlink = path =>
      Promise.make((resolve, _) => {
        unlink_raw(path, error => {
          switch Js.nullToOption(error) {
          | None => resolve(Ok())
          | Some(error) => resolve(Error(error))
          }
        })
      })

    @module("fs")
    external rename_raw: (string, string, Js.null<Js.Exn.t> => unit) => unit = "rename"
    let rename = (old, new) =>
      Promise.make((resolve, _) => {
        rename_raw(old, new, error => {
          switch Js.nullToOption(error) {
          | None => resolve(Ok())
          | Some(error) => resolve(Error(error))
          }
        })
      })

    let readFile = async filepath => {
      let fileHandle = await NodeJs.Fs.open_(filepath, NodeJs.Fs.Flag.read)
      let buffer = await NodeJs.Fs.FileHandle.readFile(fileHandle)
      await NodeJs.Fs.FileHandle.close(fileHandle)
      NodeJs.Buffer.toString(buffer)
    }

    // @module("fs")
    // external writeFile_raw: (string, NodeJs.Buffer.t, Js.null<Js.Exn.t> => unit) => unit =
    //   "writeFile"
    // let writeFile = (path, data) =>
    //   Promise.make((resolve, _) => {
    //     writeFile_raw(path, data, error => {
    //       switch Js.nullToOption(error) {
    //       | None => resolve(Ok())
    //       | Some(error) => resolve(Error(error))
    //       }
    //     })
    //   })

    let writeFile = async (filepath, string) => {
      let fileHandle = await NodeJs.Fs.open_(filepath, NodeJs.Fs.Flag.write)
      let _ = await NodeJs.Fs.FileHandle.writeFile(
        fileHandle,
        NodeJs.Buffer.fromString(string),
      )
      await NodeJs.Fs.FileHandle.close(fileHandle)
    }

    @module("fs")
    external createWriteStream: string => NodeJs.Fs.WriteStream.t = "createWriteStream"

    @module("fs")
    external createWriteStreamWithOptions: (string, {"mode": int}) => NodeJs.Fs.WriteStream.t =
      "createWriteStream"

    type rmOptions = {
      force?: bool,
      maxRetries?: int,
      recursive?: bool,
      retryDelay?: int,
    }

    @module("node:fs") @scope("promises")
    external rmWithOptions: (string, rmOptions) => promise<unit> = "rm"
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
    // OS
    | CannotDetermineOS(Js.Exn.t)

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
    // OS
    | CannotDetermineOS(exn) => "Cannot determine OS:\n" ++ Util.JsError.toString(exn)
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

  // convert all fields to a JS object-like string
  let toString = asset =>
    "{" ++
    Js.Dict.fromArray([
      ("url", asset.url),
      ("id", string_of_int(asset.id)),
      ("node_id", asset.node_id),
      ("name", asset.name),
      ("label", asset.label),
      ("content_type", asset.content_type),
      ("state", asset.state),
      ("size", string_of_int(asset.size)),
      ("created_at", asset.created_at),
      ("updated_at", asset.updated_at),
      ("browser_download_url", asset.browser_download_url),
    ])
    ->Js.Dict.entries
    ->Array.map(((k, v)) => k ++ ": " ++ v)
    ->Array.join(", ") ++ "}"

  let decode = {
    open JsonCombinators.Json.Decode
    object(field => {
      url: field.required("url", string),
      id: field.required("id", int),
      node_id: field.required("node_id", string),
      name: field.required("name", string),
      label: field.required("label", string),
      content_type: field.required("content_type", string),
      state: field.required("state", string),
      size: field.required("size", int),
      created_at: field.required("created_at", string),
      updated_at: field.required("updated_at", string),
      browser_download_url: field.required("browser_download_url", string),
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

  let chooseByName = (assets: array<t>, name): option<t> => {
    assets->Array.find(asset => asset.name == name)
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

  // convert all fields to a JS object-like string
  let toString = release =>
    "{" ++
    Js.Dict.fromArray([
      ("url", release.url),
      ("assets_url", release.assets_url),
      ("upload_url", release.upload_url),
      ("html_url", release.html_url),
      ("id", string_of_int(release.id)),
      ("node_id", release.node_id),
      ("tag_name", release.tag_name),
      ("target_commitish", release.target_commitish),
      ("name", release.name),
      ("draft", string_of_bool(release.draft)),
      ("prerelease", string_of_bool(release.prerelease)),
      ("created_at", release.created_at),
      ("published_at", release.published_at),
      ("assets", release.assets->Array.map(Asset.toString)->Array.join(", ")),
      ("tarball_url", release.tarball_url),
      ("zipball_url", release.zipball_url),
      ("body", release.body->Option.map(s => "\"" ++ s ++ "\"")->Option.getOr("null")),
    ])
    ->Js.Dict.entries
    ->Array.map(((k, v)) => k ++ ": " ++ v)
    ->Array.join(", ") ++ "}"

  let decode = {
    open JsonCombinators.Json.Decode
    object(field => {
      url: field.required("url", string),
      assets_url: field.required("assets_url", string),
      upload_url: field.required("upload_url", string),
      html_url: field.required("html_url", string),
      id: field.required("id", int),
      node_id: field.required("node_id", string),
      tag_name: field.required("tag_name", string),
      target_commitish: field.required("target_commitish", string),
      name: field.required("name", string),
      draft: field.required("draft", bool),
      prerelease: field.required("prerelease", bool),
      created_at: field.required("created_at", string),
      published_at: field.required("published_at", string),
      assets: field.required("assets", array(Asset.decode)),
      tarball_url: field.required("tarball_url", string),
      zipball_url: field.required("zipball_url", string),
      body: field.required("body", option(string)),
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
      "assets": array(Asset.encode)(release.assets),
      "tarball_url": string(release.tarball_url),
      "zipball_url": string(release.zipball_url),
      "body": option(string)(release.body),
    })
  }

  let encodeReleases = releases => {
    open JsonCombinators.Json.Encode
    array(encode)(releases)
  }

  let decodeReleases = json => {
    switch JsonCombinators.Json.decode(json, JsonCombinators.Json.Decode.array(decode)) {
    | Ok(releases) => Ok(releases)
    | Error(e) => Error(Error.ResponseDecodeError(e, json))
    }
  }

  // helper function for selecting the release based on the tag name
  let chooseByTagName = (releases: array<t>, tagName): option<t> => {
    releases->Array.find(release => release.tag_name == tagName)
  }

  // helper function for selecting the latest release
  let chooseLatest = (releases: array<t>): option<t> => {
    // fetch the latest release
    let compare = (x, y) => {
      let xTime = Js.Date.getTime(Js.Date.fromString(x.created_at))
      let yTime = Js.Date.getTime(Js.Date.fromString(y.created_at))
      compare(yTime, xTime)
    }
    let sorted = Js.Array.sortInPlaceWith(compare, releases)
    sorted[0]
  }
}

// Describes which asset to download from the GitHub releases, and what the downloaded file should be named (so that we can cache it)
module Target = {
  type t = {
    release: Release.t, // the release of the repo
    asset: Asset.t, // the asset of that release
    saveAsFileName: string, // file name of the downloaded asset
  }
}

// helper function for chmoding 744 the executable
let chmodExecutable = async path =>
  switch await NodeJs.Fs.chmod(path, ~mode=0o744) {
  | _ => Ok()
  | exception Exn.Error(_) => Error(Error.CannotChmodFile(path))
  }

// module for determining the which OS the user is using
// see https://github.com/retrohacker/getos for more 
module Platform = {
  type t = {"os": string, "dist": string, "codename": string, "release": string}

  @module
  external getos: (('e, t) => unit) => unit = "getos"

  let determine = (): promise<t> => {
    let (promise, resolve, reject) = Util.Promise.pending()
    getos((e, os) => {
      let e = Js.Nullable.toOption(e)
      switch e {
      | Some(e) => reject(e)
      | None => resolve(os)
      }
    })
    promise
  }

  // type t = Windows | MacOS | Ubuntu | Others(string, GetOs.t)

  // let determine = async () =>
  //   switch NodeJs.Os.platform() {
  //   | "darwin" => Ok(MacOS)
  //   | "linux" =>
  //     // determine the distro
  //     switch await GetOs.run() {
  //     | info =>
  //       switch info["dist"] {
  //       | "Ubuntu" => Ok(Ubuntu)
  //       | _ => Ok(Others("linux", info))
  //       }
  //     | exception Exn.Error(e) => Error(e)
  //     }
  //   | "win32" => Ok(Windows)
  //   | others =>
  //     // determine the distro
  //     switch await GetOs.run() {
  //     | info => Ok(Others(others, info))
  //     | exception Exn.Error(e) => Error(e)
  //     }
  //   }
}

module Repo = {
  type t = {
    username: string,
    repository: string,
    userAgent: string,
    globalStoragePath: string,
    chooseFromReleases: array<Release.t> => option<Target.t>,
    onDownload: Download.Event.t => unit,
    afterDownload: (
      bool, // if is from cache
      (string, Target.t),
    ) => promise<
      result<
        (string, array<string>, option<Client__LSP__Binding.executableOptions>, Target.t),
        Error.t,
      >,
    >,
    cacheInvalidateExpirationSecs: int,
    log: string => unit,
  }

  // convert all fields to a JS object-like string
  let toString = self =>
    "{" ++
    Js.Dict.fromArray([
      ("username", self.username),
      ("repository", self.repository),
      ("userAgent", self.userAgent),
      ("globalStoragePath", self.globalStoragePath),
      ("cacheInvalidateExpirationSecs", string_of_int(self.cacheInvalidateExpirationSecs)),
    ])
    ->Js.Dict.entries
    ->Array.map(((k, v)) => k ++ ": " ++ v)
    ->Array.join(", ") ++ "}"
}

module Module: {
  let get: Repo.t => promise<result<(bool, Target.t), Error.t>>
} = {
  let inFlightDownloadFileName = "in-flight.download"

  // in-flight download will be named as "in-flight.download"
  // see if "in-flight.download" already exists
  let isDownloading = async globalStoragePath => {
    try {
      let exists = switch await NodeJs.Fs.access(globalStoragePath) {
      | () => true
      | exception _ => false
      }

      if exists {
        let inFlightDownloadPath = NodeJs.Path.join2(globalStoragePath, inFlightDownloadFileName)
        let fileNames = await Nd.Fs.readdir(globalStoragePath)
        let matched = fileNames->Array.filter(fileName => fileName == inFlightDownloadPath)
        matched[0]->Option.isSome
      } else {
        // create a directory for `context.globalStoragePath` if it doesn't exist
        await NodeJs.Fs.mkdir(globalStoragePath, {mode: 0o777})
        false
      }
    } catch {
    | _ => false
    }
  }

  let downloadLanguageServer = async (repo: Repo.t, target: Target.t) => {
    let url = NodeJs.Url.make(target.asset.browser_download_url)
    let httpOptions = {
      "host": url.host,
      "path": url.pathname,
      "headers": {
        "User-Agent": repo.userAgent,
      },
    }

    let inFlightDownloadPath = NodeJs.Path.join2(repo.globalStoragePath, inFlightDownloadFileName)
    let destPath = NodeJs.Path.join2(repo.globalStoragePath, target.saveAsFileName)

    let result = switch await Download.asFile(httpOptions, inFlightDownloadPath, repo.onDownload) {
    | Error(e) => Error(Error.CannotDownload(e))
    | Ok() =>
      // suffix with ".zip" after downloaded
      switch await Nd.Fs.rename(inFlightDownloadPath, inFlightDownloadPath ++ ".zip") {
      | Error(e) => Error(Error.CannotRenameFile(e))
      | Ok() =>
        // unzip the downloaded file
        await Unzip.run(inFlightDownloadPath ++ ".zip", destPath)
        // remove the zip file
        switch await Nd.Fs.unlink(inFlightDownloadPath ++ ".zip") {
        | Error(e) => Error(Error.CannotDeleteFile(e))
        | Ok() => Ok()
        }
      }
    }

    // cleanup on error
    switch result {
    | Error(error) =>
      let remove = async path => {
        if NodeJs.Fs.existsSync(path) {
          let _ = await Nd.Fs.unlink(path)
        } else {
          ()
        }
      }
      let _ = await Promise.all([
        remove(inFlightDownloadPath),
        remove(inFlightDownloadPath ++ ".zip"),
      ])
      Error(error)
    | Ok() => Ok()
    }
  }

  // NOTE: no caching
  // timeouts after 1000ms
  let getReleasesFromGitHubRepo = async (repo: Repo.t) => {
    let httpOptions = {
      "host": "api.github.com",
      "path": "/repos/" ++ repo.username ++ "/" ++ repo.repository ++ "/releases",
      "headers": {
        "User-Agent": repo.userAgent,
      },
    }
    switch await Download.asJson(httpOptions)->Download.timeoutAfter(10000) {
    | Error(e) => Error(Error.CannotGetReleases(e))
    | Ok(json) => Release.decodeReleases(json)
    }
  }

  module Cache = {
    // util for getting stat modify time in ms
    let statModifyTime = async path =>
      switch await NodeJs.Fs.lstat(path) {
      | stat => Ok(stat.mtimeMs)
      | exception Exn.Error(_) => Error(Error.CannotStatFile(path))
      }

    let cachePath = repo => NodeJs.Path.join2(repo.Repo.globalStoragePath, "releases-cache.json")

    let isValid = async self => {
      let path = cachePath(self)

      if NodeJs.Fs.existsSync(path) {
        switch await statModifyTime(path) {
        | Error(_) => false // invalidate when there's an error
        | Ok(lastModifiedTime) =>
          let currentTime = Js.Date.now()
          // devise time difference in seconds
          let diff = int_of_float((currentTime -. lastModifiedTime) /. 1000.0)
          // cache is invalid if it is too old
          diff < self.cacheInvalidateExpirationSecs
        }
      } else {
        // the cache does not exist, hence not valid
        false
      }
    }

    let persist = async (self, releases) => {
      let json = Release.encodeReleases(releases)->Js_json.stringify
      let path = cachePath(self)
      await Nd.Fs.writeFile(path, json)
      Ok(releases) // pass it on for chaining
    }
  }

  // use cached releases instead of fetching them from GitHub, if the cached releases data is not too old (24 hrs)
  let getReleases = async repo => {
    let isValid = await Cache.isValid(repo)
    if isValid {
      // use the cached releases data
      let path = Cache.cachePath(repo)
      repo.log("[ mule ] Use cached releases data at:" ++ path)

      // read file and decode as json
      let string = await Nd.Fs.readFile(path)
      switch Js.Json.parseExn(string) {
      | json =>
        // parse the json
        switch Release.decodeReleases(json) {
        | Error(e) => Error(e)
        | Ok(releases) => Ok(releases)
        }
      | exception _ => Error(Error.JsonParseError(string))
      }
    } else {
      repo.log("[ mule ] GitHub releases cache invalidated")
      switch await getReleasesFromGitHubRepo(repo) {
      | Ok(releases) => await Cache.persist(repo, releases)
      | Error(e) => Error(e)
      }
    }
  }

  let get = async (repo: Repo.t) => {
    let ifIsDownloading = await isDownloading(repo.globalStoragePath)
    if ifIsDownloading {
      Error(Error.AlreadyDownloading)
    } else {
      switch await getReleases(repo) {
      | Error(error) => Error(error)
      | Ok(releases) =>
        switch repo.chooseFromReleases(releases) {
        | None => Error(Error.NoMatchingRelease)
        | Some(target) =>
          // don't download from GitHub if `target.fileName` already exists
          let destPath = NodeJs.Path.join2(repo.globalStoragePath, target.saveAsFileName)
          if NodeJs.Fs.existsSync(destPath) {
            repo.log("[ mule ] Used downloaded program at:" ++ destPath)
            Ok((true, target))
          } else {
            repo.log("[ mule ] Download from GitHub instead")
            switch await downloadLanguageServer(repo, target) {
            | Error(error) => Error(error)
            | Ok() => Ok((false, target))
            }
          }
        }
      }
    }
  }
}

include Module
