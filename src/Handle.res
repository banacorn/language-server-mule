type t =
  | StdIO(string, string) // name, path
  | TCP(int, string) // port, host
// | FromGitHub(string, string, string) // username, reponame, user-agent
