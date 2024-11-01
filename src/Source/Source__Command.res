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

// search for the executable in PATH with the given tool like `which` or `where.exe`
let searchWithCommand = (command, name): promise<result<string, Error.t>> =>
  Promise.make((resolve, _) => {
    // reject if the process hasn't responded for more than 1 second
    let hangTimeout = Js.Global.setTimeout(() => resolve(Error(Error.NotResponding)), 1000)

    NodeJs.ChildProcess.execWith(command ++ " " ++ name, %raw(`{shell : true}`), (
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
            Js.Exn.message(err)->Option.mapOr(
              false,
              a => Js.String.startsWith("Command failed: " ++ command ++ " " ++ name ++ "\n", a),
            )
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
  })

// search for the executable in PATH
let search = async (name): result<string, Error.t> => {
  switch NodeJs.Os.type_() {
  | "Linux"
  | "Darwin" =>
    await searchWithCommand("which", name)
  | "Windows_NT" =>
    // try `which` first, then `where.exe`
    switch await searchWithCommand("which", name) {
    | Ok(stdout) => Ok(stdout)
    | Error(_) => await searchWithCommand("where.exe", name)
    }
  | os => Error(NotSupported(os))
  }
}
