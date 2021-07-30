open Belt

module Path = Source__Path
module TCP = Source__TCP
module GitHub = Source__GitHub

type t =
  | FromPath(string) // name of the command
  | FromTCP(int, string) // port, host
  | FromGitHub(GitHub.t)

module Error = {
  type t =
    | StdIO(Path.Error.t)
    | TCP(Js.Exn.t)
    | GitHub(GitHub.Error.t)
    | NoSourcesGiven

  let toString = x => switch x {
  | StdIO(e) => Path.Error.toString(e)
  | TCP(e) => Util.JsError.toString(e)
  | GitHub(e) => GitHub.Error.toString(e)
  | NoSourcesGiven => "No source of IPC method given"
  }
}

module Module : {
  let search: t => Promise.t<result<Method.t, Error.t>>
  let searchUntilSuccess: array<t> => Promise.t<result<(t, Method.t), Error.t>>
} = {
// returns the method of IPC if successful
let search = source =>
  switch source {
  | FromPath(name) =>
    Path.search(name)
    ->Promise.mapError(e => Error.StdIO(e))
    ->Promise.mapOk(path => Method.ViaStdIO(path))
  | FromTCP(port, host) =>
    TCP.probe(port, host)
    ->Promise.mapError(e => Error.TCP(e))
    ->Promise.mapOk(() => Method.ViaTCP(port, host))
  | FromGitHub(prebuilt) =>
    GitHub.get(prebuilt)
    ->Promise.mapError(e => Error.GitHub(e))
    ->Promise.mapOk(path => Method.ViaStdIO(path))
  }

// returns both the source and method 
let searchUntilSuccess = sources => {
  let rec tryUntilSuccess = input =>
    switch input {
    | list{} => Promise.resolved(Error(Error.NoSourcesGiven))
    | list{x} => search(x)->Promise.mapOk(method => (x, method))
    | list{x, ...xs} =>
      search(x)->Promise.flatMap(result =>
        switch result {
        | Error(_) => tryUntilSuccess(xs)
        | Ok(client) => Promise.resolved(Ok(x, client))
        }
      )
    }
  sources->List.fromArray->tryUntilSuccess
}

}