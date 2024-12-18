open Mocha

describe("Process Interface", () => {
  describe("Use `echo` as the testing subject", () => {
    Async.it(
      "should trigger `close`",
      async () => {
        let process = Client.Process.make("echo", ["hello"])
        let (promise, resolve, reject) = Util.Promise.pending()
        let destructor = process->Client.Process.onOutput(
          output => {
            switch output {
            | Stdout("hello\n") => ()
            | Stdout("hello\r\n") => ()
            | Stdout(output) => reject(Js.Exn.raiseError("wrong output: " ++ output))
            | Stderr(err) => resolve(Js.Exn.raiseError("Stderr: " ++ err))
            | Event(OnExit(_, _, 0, _)) => resolve()
            | Event(event) =>
              resolve(Js.Exn.raiseError("Event: " ++ Client.Process.Event.toString(event)))
            }
          },
        )

        await promise
        destructor() // destroy the process after the test
      },
    )
  })
  describe("Use a non-existing command as the testing subject", () => {
    Async.it(
      "should trigger receive something from stderr",
      async () => {
        let process = Client.Process.make("echooo", ["hello"])
        let (promise, resolve, reject) = Util.Promise.pending()

        let destructor = process->Client.Process.onOutput(
          output => {
            switch output {
            | Stdout(output) => reject(Js.Exn.raiseError("wrong output: " ++ output))
            | Stderr(_) => resolve()
            | Event(event) =>
              reject(Js.Exn.raiseError("Event: " ++ Client.Process.Event.toString(event)))
            }
          },
        )
        await promise
        destructor() // destroy the process after the test
      },
    )
  })

//   describe("Use `node` as the testing subject", () => {
//     Async.it(
//       "should behave normally",
//       async () => {
//         switch await Source.Search.run("node") {
//         | Error(err) => reject(Js.Exn.raiseError(Search.Path.Error.toString))
//         | Ok(path) => {
//             let process = Process.make("path", [])
//             let (promise, resolve, reject) = Promise.pending()

//             let destructor = process->Process.onOutput(
//               output =>
//                 switch output {
//                 | Stdout("2") => resolve(Ok())
//                 | Stdout(_) => reject(Js.Exn.raiseError("wrong answer"))
//                 | Stderr(err) => reject(Js.Exn.raiseError("Stderr: " ++ err))
//                 | Event(event) =>
//                   reject(Js.Exn.raiseError("Event: " ++ snd(Process.Event.toString(event))))
//                 },
//             )

//             // let sent = process->Process.send("1 + 1")
//             // Assert.ok(sent)

//             // process->Process.destroy->Promise.flatMap(_ => {
//             //   method()
//             //   promise
//             // })

//             promise->Promise.tap(_ => method())
//           }

//         //   let search: t => Promise.t<result<Method.t, Error.t>>
//         //   ->Promise.mapError(Search.Path.Error.toString)
//         //   ->Promise.flatMapOk(path => {
//         //     let process = Process.make("path", [])
//         //     let (promise, resolve) = Promise.pending()

//         //     let method = process->Process.onOutput(output =>
//         //       switch output {
//         //       | Stdout("2") => resolve(Ok())
//         //       | Stdout(_) => resolve(Error("wrong answer"))
//         //       | Stderr(err) => resolve(Error("Stderr: " ++ err))
//         //       | Event(event) => resolve(Error("Event: " ++ snd(Process.Event.toString(event))))
//         //       }
//         //     )

//         // let sent = process->Process.send("1 + 1")
//         // Assert.ok(sent)

//         // process->Process.destroy->Promise.flatMap(_ => {
//         //   method()
//         //   promise
//         // })

//         //     promise->Promise.tap(_ => method())
//         //   })
//         }
//       },
//     )
//   })
})
