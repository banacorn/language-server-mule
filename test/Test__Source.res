// open BsMocha.Mocha
// module Assert = BsMocha.Assert

// open Test__Util

open Mocha

describe("Path Searching", () => {
  describe("`Source.Command.whichCommand`", () => {
    Async.it(
      "should work on itself",
      () => {
        switch Source.Command.whichCommand {
        | Error(os) => Promise.reject(Js.Exn.raiseError(os))
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

  describe("`Source.Command.search`", () => {
    Async.it(
      "should report commands that do exist",
      async () => {
        switch await Source.Command.search("npm") {
        | Error(err) => Js.Exn.raiseError(Source.Command.Error.toString(err))
        | Ok(_) => ()
        }
      },
    )

    Async.it(
      "should report `NotFound` on commands that don't exist",
      async () => {
        switch await Source.Command.search("somenonexistingprogram") {
        | Error(NotFound) => ()
        | Error(err) => raise(Js.Exn.raiseError(Source.Command.Error.toString(err)))
        | Ok(_) => ()
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
