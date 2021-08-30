open Belt

module Command = Source__Command
module File = Source__File
module TCP = Source__TCP
module GitHub = Source__GitHub

type t =
  | FromFile(string) // path of the program
  | FromCommand(string) // name of the command
  | FromTCP(int, string) // port, host
  | FromGitHub(Source__GitHub.t)
  | FromGitHub2(Source__GitHub.t)

// error from the sources
module Error = {
  type t =
    | File(string) // path of the program
    | Command(string, Command.Error.t) // name of the command, error
    | TCP(int, string, Js.Exn.t) // port, host, error
    | GitHub(GitHub.Error.t)

  let toString = error =>
    switch error {
    | File(path) => "Trying to locate \"" ++ path ++ "\" but the file does not exist"
    | Command(name, e) =>
      "Trying to find the command \"" ++ name ++ "\": " ++ Command.Error.toString(e)
    | TCP(port, host, e) =>
      "Trying to connect to " ++
      host ++
      ":" ++
      string_of_int(port) ++
      " : " ++
      Util.JsError.toString(e)
    | GitHub(e) => "Trying to download prebuilt from GitHub: " ++ GitHub.Error.toString(e)
    }
}

module Module: {
  let search: t => Promise.t<result<Method.t, Error.t>>
  // returns `Method.t` if any is found, and errors of previous searches
  let searchUntilSuccess: array<t> => Promise.t<(option<Method.t>, array<Error.t>)>
  // helper function for consuming results from `searchUntilSuccess`
  let consumeResult: ((option<Method.t>, array<Error.t>)) => result<Method.t, array<Error.t>>
} = {
  // returns the method of IPC if successful
  let search = source =>
    switch source {
    | FromFile(path) =>
      if File.probe(path) {
        Promise.resolved(Ok(Method.ViaStdIO(path, FromCommand(path))))
      } else {
        Promise.resolved(Error(Error.File(path)))
      }
    | FromCommand(name) =>
      Command.search(name)
      ->Promise.mapError(e => Error.Command(name, e))
      ->Promise.mapOk(path => Method.ViaStdIO(path, FromPath(name)))
    | FromTCP(port, host) =>
      TCP.probe(port, host)
      ->Promise.mapError(e => Error.TCP(port, host, e))
      ->Promise.mapOk(() => Method.ViaTCP(port, host, FromTCP(port, host)))
    | FromGitHub(info) =>
      GitHub.get(info)
      ->Promise.mapError(e => Error.GitHub(e))
      ->Promise.mapOk(((path, target)) => Method.ViaStdIO(
        path,
        FromGitHub(info, target.release, target.asset),
      ))
    | FromGitHub2(info) =>
      GitHub.getAgdaLanguageServer(info)
      ->Promise.mapError(e => Error.GitHub(e))
      ->Promise.mapOk(((path, args, options, target)) => Method.ViaCommand(
        path, args, options,
        FromGitHub(info, target.release, target.asset),
      ))
    }

  let searchUntilSuccess = sources => {
    let rec tryUntilSuccess = (accumErrors: list<Error.t>, input) =>
      switch input {
      | list{} => Promise.resolved((None, list{}))
      | list{x} =>
        search(x)->Promise.map(result =>
          switch result {
          | Error(e) => (None, list{e})
          | Ok(v) => (Some(v), list{})
          }
        )
      | list{x, ...xs} =>
        search(x)->Promise.flatMap(result =>
          switch result {
          | Error(e) =>
            tryUntilSuccess(accumErrors, xs)->Promise.map(((v, es)) => (v, list{e, ...es}))
          | Ok(v) => Promise.resolved((Some(v), accumErrors))
          }
        )
      }
    tryUntilSuccess(list{}, sources->List.fromArray)->Promise.map(((client, errors)) => (
      client,
      List.toArray(errors),
    ))
  }

  let consumeResult = ((result, errors)) =>
    switch result {
    | None => Error(errors)
    | Some(method) => Ok(method)
    }
}

include Module
