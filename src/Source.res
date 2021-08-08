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

// error from the sources
module Error = {
  type t =
    | File(string)
    | Command(Command.Error.t)
    | TCP(Js.Exn.t)
    | GitHub(GitHub.Error.t)
    | NoSourcesGiven

  let toString = x =>
    switch x {
    | File(e) => "File does not exist: " ++ e
    | Command(e) => Command.Error.toString(e)
    | TCP(e) => "Cannot connect with the server (" ++ Util.JsError.toString(e) ++ ")"
    | GitHub(e) => GitHub.Error.toString(e)
    | NoSourcesGiven => "No source of IPC method given"
    }
}

module Module: {
  let search: t => Promise.t<result<Method.t, Error.t>>
  let searchUntilSuccess: array<t> => Promise.t<(option<Method.t>, array<Error.t>)>
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
      ->Promise.mapError(e => Error.Command(e))
      ->Promise.mapOk(path => Method.ViaStdIO(path, FromPath(name)))
    | FromTCP(port, host) =>
      TCP.probe(port, host)
      ->Promise.mapError(e => Error.TCP(e))
      ->Promise.mapOk(() => Method.ViaTCP(port, host, FromTCP(port, host)))
    | FromGitHub(info) =>
      GitHub.get(info)
      ->Promise.mapError(e => Error.GitHub(e))
      ->Promise.mapOk(((path, target)) => Method.ViaStdIO(
        path,
        FromGitHub(info, target.release, target.asset),
      ))
    }

  let searchUntilSuccess = sources => {
    let rec tryUntilSuccess = (accumErrors: list<Error.t>, input) =>
      switch input {
      | list{} => Promise.resolved((None, list{Error.NoSourcesGiven}))
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
}

include Module
