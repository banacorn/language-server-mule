module Unzip = Source__GitHub__Unzip
module Download = Source__GitHub__Download

module Nd = {
  module Fs = {
    @module("fs")
    external mkdirSync: string => unit = "mkdirSync"

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

    @module("fs")
    external readFile_raw: (string, (Js.null<Js.Exn.t>, NodeJs.Buffer.t) => unit) => unit =
      "readFile"
    let readFile = path =>
      Promise.make((resolve, _) => {
        readFile_raw(path, (error, buffer) => {
          switch Js.nullToOption(error) {
          | None => resolve(Ok(buffer))
          | Some(error) => resolve(Error(error))
          }
        })
      })

    @module("fs")
    external writeFile_raw: (string, NodeJs.Buffer.t, Js.null<Js.Exn.t> => unit) => unit =
      "writeFile"
    let writeFile = (path, data) =>
      Promise.make((resolve, _) => {
        writeFile_raw(path, data, error => {
          switch Js.nullToOption(error) {
          | None => resolve(Ok())
          | Some(error) => resolve(Error(error))
          }
        })
      })

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
}

module Target = {
  type t = {
    release: Release.t,
    asset: Asset.t,
    saveAsFileName: string,
  }
}

// helper function for chmoding 744 the executable
let chmodExecutable = async path =>
  switch await NodeJs.Fs.chmod(path, ~mode=0o744) {
  | _ => Ok()
  | exception Exn.Error(_) => Error(Error.CannotChmodFile(path))
  }
// ->Promise.Js.fromBsPromise
// ->Promise.Js.toResult
// ->Promise.mapError(_ => Error.CannotChmodFile(path))

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
    if NodeJs.Fs.existsSync(self.globalStoragePath) {
      let inFlightDownloadPath = NodeJs.Path.join2(self.globalStoragePath, inFlightDownloadFileName)
      let fileNames = NodeJs.Fs.readdirSync(self.globalStoragePath)
      let matched = fileNames->Array.filter(fileName => fileName == inFlightDownloadPath)
      matched[0]->Option.isSome
    } else {
      // create a directory for `context.globalStoragePath` if it doesn't exist
      Nd.Fs.mkdirSync(self.globalStoragePath)
      false
    }
  }

  let downloadLanguageServer = async (self, target: Target.t) => {
    let url = Nd.Url.parse(target.asset.browser_download_url)
    let httpOptions = {
      "host": url["host"],
      "path": url["path"],
      "headers": {
        "User-Agent": self.userAgent,
      },
    }

    let inFlightDownloadPath = NodeJs.Path.join2(self.globalStoragePath, inFlightDownloadFileName)
    let destPath = NodeJs.Path.join2(self.globalStoragePath, target.saveAsFileName)

    let result = switch await Download.asFile(httpOptions, inFlightDownloadPath, self.onDownload) {
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
    | Ok() => Ok((destPath, target))
    }
  }

  // NOTE: no caching
  // timeouts after 1000ms
  let getReleasesFromGitHub = async self => {
    let httpOptions = {
      "host": "api.github.com",
      "path": "/repos/" ++ self.username ++ "/" ++ self.repository ++ "/releases",
      "headers": {
        "User-Agent": self.userAgent,
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

    let cachePath = self => NodeJs.Path.join2(self.globalStoragePath, "releases-cache.json")

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
      let json = Release.encodeReleases(releases)->Js_json.stringify->NodeJs.Buffer.fromString
      let path = cachePath(self)
      switch await Nd.Fs.writeFile(path, json) {
      | Error(e) => Error(Error.CannotCacheReleases(e))
      | Ok() => Ok(releases) // pass it on for chaining
      }
    }
  }

  // use cached releases instead of fetching them from GitHub, if the cached releases data is not too old (24 hrs)
  let getReleases = async self => {
    let isValid = await Cache.isValid(self)
    if isValid {
      // use the cached releases data
      let path = Cache.cachePath(self)
      self.log("[ mule ] Use cached releases data at:" ++ path)

      // read file and decode as json
      switch await Nd.Fs.readFile(path) {
      | Error(e) => Error(Error.CannotRenameFile(e))
      | Ok(buffer) =>
        let string = NodeJs.Buffer.toString(buffer)
        switch Js.Json.parseExn(string) {
        | json =>
          // parse the json
          switch Release.decodeReleases(json) {
          | Error(e) => Error(e)
          | Ok(releases) => Ok(releases)
          }
        | exception _ => Error(Error.JsonParseError(string))
        }
      }
    } else {
      self.log("[ mule ] GitHub releases cache invalidated")
      switch await getReleasesFromGitHub(self) {
      | Ok(releases) => await Cache.persist(self, releases)
      | Error(e) => Error(e)
      }
    }
  }

  let get = async self => {
    if isDownloading(self) {
      Error(Error.AlreadyDownloading)
    } else {
      switch await getReleases(self) {
      | Error(error) => Error(error)
      | Ok(releases) =>
        switch self.chooseFromReleases(releases) {
        | None => Error(Error.NoMatchingRelease)
        | Some(target) =>
          // don't download from GitHub if `target.fileName` already exists
          let destPath = NodeJs.Path.join2(self.globalStoragePath, target.saveAsFileName)
          if NodeJs.Fs.existsSync(destPath) {
            self.log("[ mule ] Used downloaded program at:" ++ destPath)
            await self.afterDownload(true, (destPath, target))
          } else {
            self.log("[ mule ] Download from GitHub instead")
            switch await downloadLanguageServer(self, target) {
            | Error(error) => Error(error)
            | Ok(server) => await self.afterDownload(false, server)
            }
          }
        }
      }

      // getReleases(self)
      // ->Promise.mapOk(self.chooseFromReleases)
      // ->Promise.flatMapOk(result =>
      //   switch result {
      //   | None => Promise.resolved(Error(Error.NoMatchingRelease))
      //   | Some(target) =>
      //     // don't download from GitHub if `target.fileName` already exists
      //     let destPath = NodeJs.Path.join2(self.globalStoragePath, target.saveAsFileName)
      //     if NodeJs.Fs.existsSync(destPath) {
      //       self.log("[ mule ] Used downloaded program at:" ++ destPath)
      //       self.afterDownload(true, (destPath, target))
      //     } else {
      //       self.log("[ mule ] Download from GitHub instead")
      //       downloadLanguageServer(self, target)->Promise.flatMapOk(self.afterDownload(false))
      //     }
      //   }
      // )
    }
  }
}

include Module
