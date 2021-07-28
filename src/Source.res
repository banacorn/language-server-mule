open Belt

module Path = Source__Path
module Port = Source__Port

type t =
  | FromStdIO(string) // name
  | FromTCP(int, string) // port, host
// | FromGitHub(string, string, string) // username, reponame, user-agent

module Handle = {
  type t =
    | StdIO(string, string) // name, path
    | TCP(int, string) // port, host
  // | FromGitHub(string, string, string) // username, reponame, user-agent
}

module Error = {
  type t =
    | StdIO(Path.Error.t)
    | TCP(Js.Exn.t)
    | NoSourcesGiven
}

let search = source =>
  switch source {
  | FromStdIO(name) =>
    Path.search(name)
    ->Promise.mapError(e => Error.StdIO(e))
    ->Promise.mapOk(path => Handle.StdIO(name, path))
  | FromTCP(port, host) =>
    Port.probe(port, host)
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
