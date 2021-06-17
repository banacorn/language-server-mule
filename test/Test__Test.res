open BsMocha.Mocha
module Assert = BsMocha.Assert

open Test__Util

describe("Path Searching", () => {
  describe("`whichCommand`", () => {
    Q.it("should work on itself", () => {
      switch Search__Path.whichCommand {
      | Error(os) => Promise.resolved(Error(Js.Exn.raiseError(os)))
      | Ok(command) =>
        let (promise, resolve) = Promise.pending()
        NodeJs.ChildProcess.exec(command ++ " " ++ command, (error, stdout, stderr) => {
          switch Js.Nullable.toOption(error) {
          | Some(exn) => resolve(Error(exn))
          | None =>
            if NodeJs.Buffer.toString(stderr) == "" {
              resolve(Ok(NodeJs.Buffer.toString(stdout)))
            } else {
              resolve(Error(Js.Exn.raiseError(NodeJs.Buffer.toString(stdout))))
            }
          }
        })->ignore
        promise
      }
    })
  })
  describe("`run`", () => {
    Q.it("should report commands that do exist", () => {
      Search__Path.run("npm")->Promise.mapError(err =>
        Js.Exn.raiseError(Search__Path.Error.toString(err))
      )
    })

    Q.it("should report `NotFound` on commands that don't exist", () => {
      Search__Path.run("somenonexistingprogram")->Promise.map(result =>
        switch result {
        | Error(NotFound) => Ok()
        | Error(err) => Error(Js.Exn.raiseError(Search__Path.Error.toString(err)))
        | Ok(result) => Error(Js.Exn.raiseError(result))
        }
      )
    })
  })
})
