// open BsMocha.Mocha
// module Assert = BsMocha.Assert

// open Test__Util

open Mocha

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
        | Ok(ViaCommand(command, args, options, source)) =>
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
        | Ok(ViaCommand(_)) => Exn.raiseError("Expected Error")
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
        | Ok(ViaCommand(command, args, options, source)) =>
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
        | Ok(ViaCommand(_)) => Exn.raiseError("Expected Error")
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
        | Ok(ViaCommand(_)) => Exn.raiseError("Expected ViaTCP")
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
        | Ok(ViaCommand(_)) => Exn.raiseError("Expected Error")
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
        | Ok(ViaCommand(_)) => Exn.raiseError("Expected Error")
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

  // describe("`Source.search` with `FromGitHub`", () => {
  //   Async.it(
  //     "for GitHub release that exists",
  //     async () => {
  //       open Source.GitHub



  //       let chooseFromReleases = (platform: Platform.t, releases: array<Release.t>): option<Target.t> => {
  //         let chooseRelease = (releases: array<Release.t>) => {
  //           // fetch the latest release
  //           let compare = (x: Release.t, y: Release.t) => {
  //             let xTime = Js.Date.getTime(Js.Date.fromString(x.created_at))
  //             let yTime = Js.Date.getTime(Js.Date.fromString(y.created_at))
  //             compare(yTime, xTime)
  //           }
  //           let sorted = Js.Array.sortInPlaceWith(compare, releases)
  //           sorted[0]
  //         }

  //         let chooseAsset = (release: Release.t) => {
  //           // expected suffix of asset name
  //           let expectedSuffix = switch platform {
  //           | MacOS => Some("macos.zip")
  //           | Ubuntu => Some("ubuntu.zip")
  //           | Windows => Some("windows.zip")
  //           | Others => None
  //           }

  //           // find the corresponding asset
  //           expectedSuffix
  //           ->Option.flatMap(suffix => {
  //             let matched = release.assets->Array.keep(asset => Js.String2.endsWith(asset.name, suffix))
  //             matched[0]
  //           })
  //           ->Option.map(asset => {
  //             saveAsFileName: release.tag_name ++ "-" ++ Node_process.process["platform"],
  //             Target.release,
  //             asset,
  //           })
  //         }

  //         chooseRelease(releases)->Option.flatMap(chooseAsset)
  //       }


  //       let source = Source.FromGitHub({
  //           username: "agda",
  //           repository: "agda-language-server",
  //           userAgent: "agda/agda-mode-vscode",
  //           globalStoragePath: "./",
  //           chooseFromReleases: chooseFromReleases(platform),
  //           onDownload,
  //           afterDownload,
  //           log: Js.log,
  //           cacheInvalidateExpirationSecs: 86400,
  //         })
  //       switch await Source.search(source, _, _) {
  //       | Error(err) => Exn.raiseError(Source.Error.toString(err))
  //       | Ok(ViaCommand(_)) => Exn.raiseError("Expected ViaGitHub")
  //       | Ok(ViaTCP(_)) => Exn.raiseError("Expected ViaGitHub")
  //       }
  //     },
  //   )

  //   // Async.it(
  //   //   "for GitHub release that doesn't exist",
  //   //   async () => {
  //   //     switch await Source.search(FromGitHub(Source.GitHub.make("rescript", "rescript-compiler-non-existing"))) {
  //   //     | Error(error) =>
  //   //       Assert.deepEqual(
  //   //         Source.Error.toString(error),
  //   //         "Trying to download prebuilt from GitHub: Error: Not Found",
  //   //       )
  //   //     | Ok(ViaCommand(_)) => Exn.raiseError("Expected Error")
  //   //     | Ok(ViaTCP(_)) => Exn.raiseError("Expected Error")
  //   //     }
  //   //   },
  //   // )
  // })

  describe("`Source.Command.whichCommand`", () => {
    Async.it(
      "should work on itself",
      () => {
        switch Source.Command.whichCommand {
        | Error(os) => Promise.reject(raise(Js.Exn.raiseError(os)))
        | Ok(command) =>
          Promise.make(
            (resolve, reject) => {
              NodeJs.ChildProcess.exec(
                command ++ " " ++ command,
                (error, stdout, stderr) => {
                  switch Js.Nullable.toOption(error) {
                  | Some(exn) => reject(Util.JsError.toString(exn))
                  | None =>
                    if NodeJs.Buffer.toString(stderr) == "" {
                      resolve()
                    } else {
                      reject(NodeJs.Buffer.toString(stdout))
                    }
                  }
                },
              )->ignore
            },
          )
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
