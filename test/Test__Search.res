open BsMocha.Mocha
module Assert = BsMocha.Assert

open Test__Util

describe("Path Searching", () => {
  describe("`whichCommand`", () => {
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
  describe("`run`", () => {
    Q.it("should report commands that do exist", () => {
      Search.Path.run("npm")->Promise.mapError(err =>
        Js.Exn.raiseError(Search.Path.Error.toString(err))
      )
    })

    Q.it("should report `NotFound` on commands that don't exist", () => {
      Search.Path.run("somenonexistingprogram")->Promise.map(result =>
        switch result {
        | Error(NotFound) => Ok()
        | Error(err) => Error(Js.Exn.raiseError(Search.Path.Error.toString(err)))
        | Ok(result) => Error(Js.Exn.raiseError(result))
        }
      )
    })
  })
})