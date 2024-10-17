// module for searching executables in PATH
module Error = {
  type t =
    | NotResponding // the searching takes more than 1 second
    | NotSupported(string) // with OS name
    | OnError(Js.Exn.t)
    | OnStderr(string)
    | NotFound

  let toString = x =>
    switch x {
    | NotResponding => "Took more than 1 second to when looking for the executable"
    | NotSupported(os) => "Path searching is not supported on \"" ++ os ++ "\""
    | OnError(exn) => "Got error when looking for the executable: " ++ Util.JsError.toString(exn)
    | OnStderr(msg) => "Got something from the stderr when looking for the executable: " ++ msg
    | NotFound => "Cannot find the executable on PATH"
    }
}

// the command we use for searching the path
let whichCommand = switch NodeJs.Os.type_() {
| "Linux"
| "Darwin" =>
  Ok("which")
| "Windows_NT" => Ok("where.exe")
| os => Error(os)
}

let search = (name): promise<result<string, Error.t>> =>
  Promise.make((resolve, _) => {
    // reject if the process hasn't responded for more than 1 second
    let hangTimeout = Js.Global.setTimeout(() => resolve(Error(Error.NotResponding)), 1000)

    switch whichCommand {
    | Error(os) => resolve(Error(NotSupported(os)))
    | Ok(whichCommand) =>
      NodeJs.ChildProcess.execWith(whichCommand ++ " " ++ name, %raw(`{shell : true}`), (
        error,
        stdout,
        stderr,
      ) => {
        // clear timeout as the process has responded
        Js.Global.clearTimeout(hangTimeout)
        // error
        error
        ->Js.Nullable.toOption
        ->Option.forEach(
          err => {
            let isNotFound =
              Js.Exn.message(err)->Option.mapOr(false, a => Js.String.startsWith("Command failed: " ++ whichCommand ++ " " ++ name ++ "\n", a))
            if isNotFound {
              resolve(Error(NotFound))
            } else {
              resolve(Error(OnError(err)))
            }
          },
        )

        // stderr
        let stderr = NodeJs.Buffer.toString(stderr)
        if stderr != "" {
          resolve(Error(OnStderr(stderr)))
        }

        // stdout
        let stdout = NodeJs.Buffer.toString(stdout)->String.trim
        if stdout == "" || stdout == name ++ " not found" {
          resolve(Error(NotFound))
        } else {
          resolve(Ok(stdout))
        }
      })->ignore
    }
  })
