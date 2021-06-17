open BsMocha.Mocha
module Assert = BsMocha.Assert

open Test__Util

describe("Process Interface", () => {
  describe("Use `echo` as the testing subject", () => {
    Q.it("should trigger `close`", () => {
      let process = Process.make("echo", ["hello"])
      let (promise, resolve) = Promise.pending()

      let handle = process->Process.onOutput(output => {
        // Js.log(("OUTPUT", output))
        switch output {
        | Stdout("hello\n") => ()
        | Stdout(output) => resolve(Error("wrong output: " ++ output))
        | Stderr(err) => resolve(Error("Stderr: " ++ err))
        | Event(OnExit(_, _, 0, _)) => resolve(Ok())
        | Event(event) => resolve(Error("Event: " ++ Process.Event.toString(event)))
        }
      })
      promise->Promise.tap(_ => handle())
    })
  })
  describe("Use a non-existing command as the testing subject", () => {
    Q.it("should trigger receive something from stderr", () => {
      let process = Process.make("echooo", ["hello"])
      let (promise, resolve) = Promise.pending()

      let handle = process->Process.onOutput(output => {
        switch output {
        | Stdout(output) => resolve(Error("wrong output: " ++ output))
        | Stderr(_) => resolve(Ok())
        | Event(event) => resolve(Error("Event: " ++ Process.Event.toString(event)))
        }
      })
      promise->Promise.tap(_ => handle())
    })
  })

  // describe("Use `node` as the testing subject", () => {
  //   Q.it("should behave normally", () => {
  //     Search.Path.run("node")
  //     ->Promise.mapError(Search.Path.Error.toString)
  //     ->Promise.flatMapOk(path => {
  //       let process = Process.make("path", [])
  //       let (promise, resolve) = Promise.pending()

  //       let handle = process->Process.onOutput(output =>
  //         switch output {
  //         | Stdout("2") => resolve(Ok())
  //         | Stdout(_) => resolve(Error("wrong answer"))
  //         | Stderr(err) => resolve(Error("Stderr: " ++ err))
  //         | Event(event) => resolve(Error("Event: " ++ snd(Process.Event.toString(event))))
  //         }
  //       )

  //       // let sent = process->Process.send("1 + 1")
  //       // Assert.ok(sent)

  //       // process->Process.destroy->Promise.flatMap(_ => {
  //       //   Js.log("destroyed")
  //       //   handle()
  //       //   promise
  //       // })

  //       promise->Promise.tap(_ => handle())
  //     })
  //   })
  // })
})
