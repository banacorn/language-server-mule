open Belt

module Path = Source__Path
module TCP = Source__TCP
module GitHub = Source__GitHub

type t =
  | FromPath(string) // name of the command
  | FromTCP(int, string) // port, host
  | FromGitHub(Source__GitHub.t)

module Error = {
  type t =
    | StdIO(Path.Error.t)
    | TCP(Js.Exn.t)
    | GitHub(GitHub.Error.t)
    | NoSourcesGiven

  let toString = x =>
    switch x {
    | StdIO(e) => Path.Error.toString(e)
    | TCP(e) => Util.JsError.toString(e)
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
    | FromPath(name) =>
      Path.search(name)
      ->Promise.mapError(e => Error.StdIO(e))
      ->Promise.mapOk(path => Method.ViaStdIO(path, FromPath(name)))
    | FromTCP(port, host) =>
      TCP.probe(port, host)
      ->Promise.mapError(e => Error.TCP(e))
      ->Promise.mapOk(() => Method.ViaTCP(port, host, FromTCP(port, host)))
    | FromGitHub(prebuilt) =>
      GitHub.get(prebuilt)
      ->Promise.mapError(e => Error.GitHub(e))
      ->Promise.mapOk(((path, target)) => Method.ViaStdIO(
        path,
        FromGitHub(prebuilt, target.release, target.asset),
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
