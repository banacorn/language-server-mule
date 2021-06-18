open BsMocha.Mocha
module Assert = BsMocha.Assert

open Test__Util

describe("Path Searching", () => {
  describe("`Search.Path.whichCommand`", () => {
    Q.it("should work on itself", () => {
      switch Search.Path.whichCommand {
      | Error(os) => Promise.resolved(Error(os))
      | Ok(command) =>
        let (promise, resolve) = Promise.pending()
        NodeJs.ChildProcess.exec(command ++ " " ++ command, (error, stdout, stderr) => {
          switch Js.Nullable.toOption(error) {
          | Some(exn) => resolve(Error(Util.JsError.toString(exn)))
          | None =>
            if NodeJs.Buffer.toString(stderr) == "" {
              resolve(Ok(NodeJs.Buffer.toString(stdout)))
            } else {
              resolve(Error(NodeJs.Buffer.toString(stdout)))
            }
          }
        })->ignore
        promise
      }
    })
  })
  describe("`Search.Path.run`", () => {
    Q.it("should report commands that do exist", () => {
      Search.Path.search("npm")->Promise.mapError(err =>
        Js.Exn.raiseError(Search.Path.Error.toString(err))
      )
    })

    Q.it("should report `NotFound` on commands that don't exist", () => {
      Search.Path.search("somenonexistingprogram")->Promise.map(result =>
        switch result {
        | Error(NotFound) => Ok()
        | Error(err) => Error(Js.Exn.raiseError(Search.Path.Error.toString(err)))
        | Ok(result) => Error(Js.Exn.raiseError(result))
        }
      )
    })
  })
})

describe("Port Probing", () => {
  describe("`Search.Port.probe`", () => {
    Q.it("should report Ok on the port that is available", () => {
      //
      let tempServer =
        NodeJs.Net.TcpServer.make()->NodeJs.Net.TcpServer.listen(
          ~port=23456,
          ~host="localhost",
          ~callback=() => (),
        )

      Search.Port.probe(23456, "localhost")
      ->Promise.mapError(Util.JsError.toString)
      ->Promise.tap(_ => NodeJs.Net.TcpServer.close(tempServer, ~callback=_ => ())->ignore)
    })
    Q.it("should report Error on ports that are not available", () => {
      Search.Port.probe(12345, "localhost")->Promise.map(result =>
        switch result {
        | Error(_exn) => Ok()
        | Ok() => Error("Port should not be available")
        }
      )
    })
  })
})
