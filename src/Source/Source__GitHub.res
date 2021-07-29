module Unzip = Source__Prebuilt__Unzip
module Download = Source__Prebuilt__Download

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
    | Downloading
    // parsing
    | ResponseParseError(string)
    | ResponseDecodeError(string, Js.Json.t)
    // network
    | CannotDownload(Download.Error.t)
    //
    | NoMatchingRelease
    | NotSupportedOS(string)
    // file system
    | CannotDeleteFile(Js.Exn.t)
    | CannotRenameFile(Js.Exn.t)
    | CannotUnzipFile(Unzip.Error.t)

  let toString = x =>
    switch x {
    | Downloading => "Already downloading"
    | ResponseParseError(raw) => "Cannot parse release metadata from GitHub:\n" ++ raw
    | ResponseDecodeError(msg, _) => "Cannot decode release metadata JSON from GitHub:\n" ++ msg
    // network
    | CannotDownload(error) => Download.Error.toString(error)
    // metadata
    | NoMatchingRelease => "Cannot matching release from GitHub"
    | NotSupportedOS(os) => "Cannot find prebuilt for " ++ os
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
    srcUrl: string,
    fileName: string,
  }
}

// module Metadata = {
//   type t = {
//     srcUrl: string,
//     fileName: string
//   }

//   let getCurrentVersion = (
//     username,
//     repository,
//     userAgent,
//     globalStoragePath,
//     chooseFromReleases: array<Release.t> => option<t>,
//   ): Promise.t<result<option<t>, Error.t>> => {
//     // let getCurrentRelease = (releases: array<Release.t>) => {
//     //   open Belt
//     //   let matched = releases->Array.keep(release => release.tagName == version)
//     //   switch matched[0] {
//     //   | None => Promise.resolved(Error(Error.NoMatchingRelease))
//     //   | Some(release) => Promise.resolved(Ok(release))
//     //   }
//     // }

//     // let toDestPath = (globalStoragePath, release: Release.t, asset: Asset.t) => {
//     //   // take the "macos" part from names like "gcl-macos.zip"
//     //   let osName = Js.String2.slice(asset.name, ~from=4, ~to_=-4)
//     //   // the final path to store the language server
//     //   Node_path.join2(globalStoragePath, "gcl-" ++ release.tagName ++ "-" ++ osName)
//     // }

//     // let getCurrentAsset = (release: Release.t) => {
//     //   open Belt
//     //   // expected asset name
//     //   let os = Node_process.process["platform"]
//     //   let expectedName = switch os {
//     //   | "darwin" => Ok("gcl-macos.zip")
//     //   | "linux" => Ok("gcl-ubuntu.zip")
//     //   | "win32" => Ok("gcl-windows.zip")
//     //   | others => Error(Error.NotSupportedOS(others))
//     //   }

//     //   // find the corresponding asset
//     //   expectedName
//     //   ->Result.flatMap(name => {
//     //     let matched = release.assets->Array.keep(asset => asset.name == name)
//     //     switch matched[0] {
//     //     | None => Error(Error.NotSupportedOS(os))
//     //     | Some(asset) =>
//     //       Ok({
//     //         srcUrl: asset.url,
//     //         destPath: toDestPath(globalStoragePath, release, asset),
//     //         version: release.tagName,
//     //       })
//     //     }
//     //   })
//     //   ->Promise.resolved
//     // }

//     Release.getReleasesFromGitHub(username, repository, userAgent)
//     ->Promise.mapOk(chooseFromReleases)
//     ->Promise.mapOk(x =>
//       switch x {
//       | None => None
//       | Some((release, asset)) => Some({
//             srcUrl: asset.url,
//             destPath: toDestPath(globalStoragePath, release, asset),
//             version: release.tagName,
//           })
//       }
//     )
//   }
// }

open Belt 

module Module: {
  type t = {
    username: string,
    repository: string,
    userAgent: string,
    globalStoragePath: string,
    chooseFromReleases: array<Release.t> => option<Target.t>,
  }
  let get: t => Promise.t<result<string, Error.t>>
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
    // suffix with ".download" whilst downloading

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
      Nd.Fs.unlink(destPath ++ ".zip")->Promise.mapError(e => Error.CannotDeleteFile(e))
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
          remove(destPath ++ ".zip.download"),
          remove(destPath ++ ".zip"),
        ])->Promise.map(_ => Error(error))
      | Ok() => Promise.resolved(Ok(destPath))
      }
    )
  }

  // let checkExistingDownload = (globalStoragePath, version) => {
  //   // create a directory for `context.globalStoragePath` if it doesn't exist
  //   if !Node.Fs.existsSync(globalStoragePath) {
  //     Nd.Fs.mkdirSync(globalStoragePath)
  //   }

  //   // devise the expected file name of the language server and see if the OS is supported
  //   let getExpectedFileName = {
  //     switch Node_process.process["platform"] {
  //     | "darwin" => Ok("gcl-" ++ version ++ "-macos")
  //     | "linux" => Ok("gcl-" ++ version ++ "-ubuntu")
  //     | "win32" => Ok("gcl-" ++ version ++ "-windows")
  //     | others => Error(Error.NotSupportedOS(others))
  //     }
  //   }

  //   getExpectedFileName->Belt.Result.map(expected => {
  //     // find the current asset from `context.globalStoragePath`
  //     let fileNames = NodeJs.Fs.readdirSync(globalStoragePath)
  //     let downloaded = fileNames->Js.Array2.some(actual => expected == actual)

  //     if downloaded {
  //       let path = NodeJs.Path.join2(globalStoragePath, expected)
  //       Some(path)
  //     } else {
  //       None
  //     }
  //   })
  // }

  let get = self => {
    if isDownloading(self) {
      Promise.resolved(Error(Error.Downloading))
    } else {
      Release.getReleasesFromGitHub(self.username, self.repository, self.userAgent)
      ->Promise.mapOk(self.chooseFromReleases)
      ->Promise.flatMapOk(result =>
        switch result {
        | None => Promise.resolved(Error(Error.NoMatchingRelease))
        | Some(target) => downloadLanguageServer(self, target)
        }
      )
    }
    // not initialized yet
    // switch checkExistingDownload(self.globalStoragePath, self.expectedVersion) {
    // | Ok(None) =>
    //   let (promise, resolve) = Promise.pending()
    //   state := Some(InFlight(promise))
    //   Release.getReleasesFromGitHub(self.username, self.repository, self.userAgent)
    //   ->Promise.mapOk(self.chooseFromReleases)
    //   ->Promise.flatMapOk(result =>
    //     switch result {
    //     | None => Promise.resolved(Error(Error.NoMatchingRelease))
    //     | Some(target) => downloadLanguageServer(self.globalStoragePath, target)
    //     }
    //   )
    //   ->Promise.tap(resolve)
    // | Ok(Some(path)) =>
    //   state := Some(Downloaded(path))
    //   Promise.resolved(Ok(path))
    // | Error(error) => Promise.resolved(Error(error))
    // }
    // | Some(Downloaded(path)) => Promise.resolved(Ok(path))
    // // returns a promise that will be resolved once the download's been completed
    // | Some(InFlight(promise)) => promise
    // }
  }
}

include Module
