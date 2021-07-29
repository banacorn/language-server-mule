// Means of Inter-process communication
type t =
  | ViaStdIO(string) // path
  | ViaTCP(int, string) // port, host
