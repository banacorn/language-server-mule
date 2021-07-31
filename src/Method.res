// Source of the language server or process
module Source = {
  type t =
    | FromPath(string) // name of the command
    | FromTCP(int, string) // port, host
    | FromGitHub(Source__GitHub.t)
}

// Means of Inter-process communication
type t =
  | ViaStdIO(Source.t, string) // path
  | ViaTCP(Source.t, int, string) // port, host
