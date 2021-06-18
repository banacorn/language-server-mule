open Belt

module Error = {
  type t =
    | StdIO(Search.Path.Error.t)
    | TCP(Js.Exn.t)
    | NoSourcesGiven
}

module Source = {
  type t =
    | FromStdIO(string) // name
    | FromTCP(int, string) // port, host
  // | FromGitHub(string, string, string) // username, reponame, user-agent
}

module Module: {
  let run: array<Source.t> => Promise.t<result<LSP.ServerOptions.t, Error.t>>
} = {
  let toLSPServerOptions = source =>
    switch source {
    | Source.FromStdIO(name) =>
      Search.Path.search(name)
      ->Promise.mapError(e => Error.StdIO(e))
      ->Promise.mapOk(path => LSP.ServerOptions.makeWithCommand(path))
    | FromTCP(port, host) =>
      Search.Port.probe(port, host)
      ->Promise.mapError(e => Error.TCP(e))
      ->Promise.mapOk(() => LSP.ServerOptions.makeWithStreamInfo(port, host))
    }

  let rec tryUntilSuccess = sources =>
    switch sources {
    | list{} => Promise.resolved(Error(Error.NoSourcesGiven))
    | list{x} => toLSPServerOptions(x)
    | list{x, ...xs} =>
      toLSPServerOptions(x)->Promise.flatMap(result =>
        switch result {
        | Error(_) => tryUntilSuccess(xs)
        | Ok(client) => Promise.resolved(Ok(client))
        }
      )
    }
  let run = xs => xs->List.fromArray->tryUntilSuccess
}
