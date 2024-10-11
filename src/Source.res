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
  let search = async source =>
    switch source {
    | FromFile(path) =>
      if File.probe(path) {
        Ok(Method.ViaCommand(path, [], None, FromCommand(path)))
      } else {
        Error(Error.File(path))
      }
    | FromCommand(name) =>
      switch await Command.search(name) {
      | Error(e) => Error(Error.Command(name, e))
      | Ok(path) => Ok(Method.ViaCommand(path, [], None, FromPath(name)))
      }
    | FromTCP(port, host) =>
      switch await TCP.probe(port, host) {
      | Error(e) => Error(Error.TCP(port, host, e))
      | Ok() => Ok(Method.ViaTCP(port, host, FromTCP(port, host)))
      }
    | FromGitHub(info) =>
      switch await GitHub.get(info) {
      | Error(e) => Error(Error.GitHub(e))
      | Ok((path, args, options, target)) =>
        Ok(Method.ViaCommand(path, args, options, FromGitHub(info, target.release, target.asset)))
      }
    }

  let searchUntilSuccess = async sources => {
    let rec tryUntilSuccess = async (accumErrors: list<Error.t>, input) =>
      switch input {
      | list{} => (None, list{})
      | list{x} =>
        switch await search(x) {
        | Error(e) => (None, list{e})
        | Ok(v) => (Some(v), list{})
        }
      | list{x, ...xs} =>
        switch await search(x) {
        | Error(e) =>
          let (v, es) = await tryUntilSuccess(accumErrors, xs)
          (v, list{e, ...es})
        | Ok(v) => (Some(v), accumErrors)
        }
      }
    let (client, errors) = await tryUntilSuccess(list{}, sources->List.fromArray)
    (client, List.toArray(errors))
  }

  let consumeResult = ((result, errors)) =>
    switch result {
    | None => Error(errors)
    | Some(method) => Ok(method)
    }
}

include Module
