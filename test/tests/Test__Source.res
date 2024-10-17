open Mocha
open Test__Util

describe("Path Searching", () => {
  describe("`Source.search` with `FromFile`", () => {
    Async.before(
      async () => {
        let file = await NodeJs.Fs.open_("temp", NodeJs.Fs.Flag.write)
        await NodeJs.Fs.FileHandle.writeFile(file, NodeJs.Buffer.fromString("test"))
      },
    )

    Async.it(
      "for file that exists",
      async () => {
        switch await Source.search(FromFile("temp")) {
        | Error(err) => Exn.raiseError(Source.Error.toString(err))
        | Ok(ViaPipe(command, args, options, source)) =>
          Assert.deepEqual(command, "temp")
          Assert.deepEqual(args, [])
          Assert.deepEqual(options, None)
          Assert.deepEqual(source, FromFile("temp"))
        | Ok(ViaTCP(_)) => Exn.raiseError("Expected ViaCommand")
        }
      },
    )

    Async.it(
      "for file that doesn't exist",
      async () => {
        switch await Source.search(FromFile("temp-non-existing")) {
        | Error(error) =>
          Assert.deepEqual(
            Source.Error.toString(error),
            "Trying to locate \"temp-non-existing\" but the file does not exist",
          )
        | Ok(ViaPipe(_)) => Exn.raiseError("Expected Error")
        | Ok(ViaTCP(_)) => Exn.raiseError("Expected Error")
        }
      },
    )

    Async.after(
      async () => {
        NodeJs.Fs.unlinkSync("temp")
      },
    )
  })

  describe("`Source.search` with `FromCommand`", () => {
    Async.it(
      "for command that exists",
      async () => {
        switch await Source.search(FromCommand("which")) {
        | Error(err) => Exn.raiseError(Source.Error.toString(err))
        | Ok(ViaPipe(command, args, options, source)) =>
          let path = switch await Source.Command.search("which") {
          | Error(err) => Js.Exn.raiseError(Source.Command.Error.toString(err))
          | Ok(path) => path
          }

          Assert.deepEqual(command, path)
          Assert.deepEqual(args, [])
          Assert.deepEqual(options, None)
          Assert.deepEqual(source, FromCommand("which"))
        | Ok(ViaTCP(_)) => Exn.raiseError("Expected ViaCommand")
        }
      },
    )

    Async.it(
      "for command that doesn't exist",
      async () => {
        switch await Source.search(FromCommand("temp-non-existing")) {
        | Error(error) =>
          Assert.deepEqual(
            Source.Error.toString(error),
            "Trying to find the command \"temp-non-existing\": Cannot find the executable on PATH",
          )
        | Ok(ViaPipe(_)) => Exn.raiseError("Expected Error")
        | Ok(ViaTCP(_)) => Exn.raiseError("Expected Error")
        }
      },
    )
  })

  describe("`Source.search` with `FromTCP`", () => {
    open NodeJs.Net

    let serverRef = ref(None)

    Async.before(
      async () => {
        let server = TcpServer.make()
        server->TcpServer.listen(~port=23456, ~host="localhost", ~callback=_ => ())->ignore
        serverRef := Some(server)
      },
    )

    Async.it(
      "for TCP server that exists",
      async () => {
        switch await Source.search(FromTCP(23456, "localhost")) {
        | Error(err) => Exn.raiseError(Source.Error.toString(err))
        | Ok(ViaPipe(_)) => Exn.raiseError("Expected ViaTCP")
        | Ok(ViaTCP(port, host, source)) =>
          Assert.deepEqual(port, 23456)
          Assert.deepEqual(host, "localhost")
          Assert.deepEqual(source, FromTCP(23456, "localhost"))
        }
      },
    )

    Async.it(
      "for TCP server that doesn't exist",
      async () => {
        switch await Source.search(FromTCP(23457, "localhost")) {
        | Error(error) =>
          Assert.deepEqual(
            Source.Error.toString(error),
            "Trying to connect to localhost:23457 : AggregateError",
          )
        | Ok(ViaPipe(_)) => Exn.raiseError("Expected Error")
        | Ok(ViaTCP(_)) => Exn.raiseError("Expected Error")
        }
      },
    )

    Async.it(
      "for TCP server that doesn't exist",
      async () => {
        switch await Source.search(FromTCP(23457, "remotehost")) {
        | Error(error) =>
          Assert.deepEqual(
            Source.Error.toString(error),
            "Trying to connect to remotehost:23457 : Error: getaddrinfo ENOTFOUND remotehost",
          )
        | Ok(ViaPipe(_)) => Exn.raiseError("Expected Error")
        | Ok(ViaTCP(_)) => Exn.raiseError("Expected Error")
        }
      },
    )

    Async.after(
      async () => {
        switch serverRef.contents {
        | Some(server) =>
          TcpServer.close(server, ~callback=_ => ())->ignore
          serverRef := None
        | None => Exn.raiseError("Server not found")
        }
      },
    )
  })

  describe("`Source.search` with `FromGitHub`", () => {
    // set timeout to 60 seconds because we are downloading stuff
    This.timeout(60000)
    // so that we can delete the whole directory after the test
    let downloadDirRef = ref(None)

    Async.it(
      "for GitHub release that exists",
      async () => {
        open Source.GitHub

        let chooseFromReleases = (platform: Platform.t, releases: array<Release.t>): option<
          Target.t,
        > => {
          let _chooseLatestRelease = (releases: array<Release.t>) => {
            // fetch the latest release
            let compare = (x: Release.t, y: Release.t) => {
              let xTime = Js.Date.getTime(Js.Date.fromString(x.created_at))
              let yTime = Js.Date.getTime(Js.Date.fromString(y.created_at))
              compare(yTime, xTime)
            }
            let sorted = Js.Array.sortInPlaceWith(compare, releases)
            sorted[0]
          }

          let v0_2_6_4_0_3 = releases->Array.find(release => release.tag_name == "v0.2.6.4.0.3")

          let chooseFromAsset = (release: Release.t) => {
            // expected suffix of asset name
            let expectedSuffix = switch platform {
            | MacOS => Some("macos.zip")
            | Ubuntu => Some("ubuntu.zip")
            | Windows => Some("windows.zip")
            | Others(_) => None
            }

            // find the corresponding asset
            expectedSuffix
            ->Option.flatMap(
              suffix => {
                let matched =
                  release.assets->Array.filter(asset => Js.String2.endsWith(asset.name, suffix))
                matched[0]
              },
            )
            ->Option.map(
              asset => {
                Target.saveAsFileName: release.tag_name ++ "-" ++ NodeJs.Os.platform(),
                release,
                asset,
              },
            )
          }

          // chooseLatestRelease(releases)->Option.flatMap(chooseAsset)
          v0_2_6_4_0_3->Option.flatMap(chooseFromAsset)
        }

        let afterDownload = async (fromCached, (path, target)) => {
          let execPath = NodeJs.Path.join2(path, "als")
          // include "Agda_datadir" in the environment variable
          let options = {
            let assetPath = NodeJs.Path.join2(path, "data")
            let env = Js.Dict.fromArray([("Agda_datadir", assetPath)])
            {
              LanguageServerMule.Client__LSP__Binding.env: env,
            }
          }
          // because it should've been chmod'ed after download
          // because there's no need of chmod'ing on Windows
          let shouldChmod777 = !fromCached && NodeJs.Os.platform() != "win32"
          if shouldChmod777 {
            let _ = await chmodExecutable(execPath)
          }

          // store the download path for cleanup
          downloadDirRef := Some(path)

          Ok((execPath, [], Some(options), target))
        }

        let platform = switch await Platform.determine() {
        | Ok(platform) => platform
        | Error(exn) => Exn.raiseError(Util.JsError.toString(exn))
        }

        let repo = {
          Repo.username: "agda",
          repository: "agda-language-server",
          userAgent: "agda/agda-mode-vscode",
          globalStoragePath: "./",
          chooseFromReleases: chooseFromReleases(platform, ...),
          onDownload: _ => (),
          afterDownload,
          log: x => Js.log(x),
          cacheInvalidateExpirationSecs: 86400,
        }

        switch await Source.search(Source.FromGitHub(repo)) {
        | Error(err) => Exn.raiseError(Source.Error.toString(err))
        | Ok(ViaPipe(command, args, options, source)) =>
          let downloadDir = switch downloadDirRef.contents {
          | Some(path) => path
          | None => Exn.raiseError("Expected download path")
          }

          // `command` should be the path to the download directory + "/als"
          Assert.deepEqual(command, NodeJs.Path.join2(downloadDir, "als"))
          // no arguments supplied in this test case
          Assert.deepEqual(args, [])
          // `options` should include "Agda_datadir" in the environment variable
          let expectedOptions = Some({
            LanguageServerMule.Client__LSP__Binding.env: Dict.fromArray([
              ("Agda_datadir", NodeJs.Path.join2(downloadDir, "data")),
            ]),
          })
          Assert.deepEqual(options, expectedOptions)

          switch source {
          | FromGitHub(repo, release, _) =>
            Assert.deepEqual(repo.username, "agda")
            Assert.deepEqual(repo.repository, "agda-language-server")
            Assert.deepEqual(repo.userAgent, "agda/agda-mode-vscode")
            Assert.deepEqual(repo.globalStoragePath, "./")
            Assert.deepEqual(release.tag_name, "v0.2.6.4.0.3")
          | _ => Exn.raiseError("Expected FromGitHub")
          }

        | Ok(ViaTCP(_)) => Exn.raiseError("Expected ViaPipe")
        }
      },
    )

    Async.after(
      async () => {
        try {
          NodeJs.Fs.unlinkSync("releases-cache.json")
          NodeJs.Fs.unlinkSync("in-flight.download")
        } catch {
        | _ => ()
        }
        switch downloadDirRef.contents {
        | Some(path) => await Source__GitHub.Nd.Fs.rmWithOptions(path, {recursive: true})
        | None => ()
        }
      },
    )
  })
})

describe("Port Probing", () => {
  describe("`Source.Port.probe`", () => {
    Async.it(
      "should report Ok on the port that is available",
      async () => {
        let tempServer = NodeJs.Net.TcpServer.make()
        await Promise.make(
          (resolve, _) => {
            tempServer
            ->NodeJs.Net.TcpServer.listen(~port=23456, ~host="localhost", ~callback=resolve)
            ->ignore
          },
        )

        let _ = await Source.TCP.probe(23456, "localhost")
        NodeJs.Net.TcpServer.close(tempServer, ~callback=_ => ())->ignore
      },
    )

    Async.it(
      "should report Error on ports that are not available",
      async () => {
        switch await Source.TCP.probe(12345, "localhost") {
        | Error(_exn) => ()
        | Ok() => raise(Js.Exn.raiseError("Port should not be available"))
        }
      },
    )
  })
})