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
  let search: Source.t => Promise.promise<Promise.result<Handle.t, Error.t>>
  let searchUntilSuccess: array<Source.t> => Promise.promise<Promise.result<Handle.t, Error.t>>

  let asLanguageServer: (string, string, Handle.t) => Promise.t<result<LSPClient.t, LSPClient.Error.t>>
} = {
  let search = source =>
    switch source {
    | Source.FromStdIO(name) =>
      Search.Path.search(name)
      ->Promise.mapError(e => Error.StdIO(e))
      ->Promise.mapOk(path => Handle.StdIO(name, path))
    | FromTCP(port, host) =>
      Search.Port.probe(port, host)
      ->Promise.mapError(e => Error.TCP(e))
      ->Promise.mapOk(() => Handle.TCP(port, host))
    }

  let searchUntilSuccess = sources => {
    let rec tryUntilSuccess = input =>
      switch input {
      | list{} => Promise.resolved(Error(Error.NoSourcesGiven))
      | list{x} => search(x)
      | list{x, ...xs} =>
        search(x)->Promise.flatMap(result =>
          switch result {
          | Error(_) => tryUntilSuccess(xs)
          | Ok(client) => Promise.resolved(Ok(client))
          }
        )
      }
    sources->List.fromArray->tryUntilSuccess
  }

  let asLanguageServer = LSPClient.make
}
