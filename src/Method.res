// like Source.t but with more info for display 
type source =
  | FromFile(string) // path of the program 
  | FromCommand(string) // name of the command
  | FromTCP(int, string) // port, host
  | FromGitHub(Source__GitHub.t, Source__GitHub.Release.t, Source__GitHub.Asset.t)

// Means of Inter-process communication
type t =
  | ViaCommand(string, array<string>, option<Client__LSP__Binding.ExecutableOptions.t>, source) // command, args, options, source
  | ViaTCP(int, string, source) // port, host
