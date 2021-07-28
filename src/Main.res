module Module: {
  let search: Source.t => Promise.promise<Promise.result<Source.Handle.t, Source.Error.t>>
  let searchUntilSuccess: array<Source.t> => Promise.promise<Promise.result<Source.Handle.t, Source.Error.t>>

  let asLanguageServer: (string, string, Source.Handle.t) => Promise.t<result<Client.LSP.t, Client.LSP.Error.t>>
} = {
  let search = Source.search
  let searchUntilSuccess = Source.searchUntilSuccess
  let asLanguageServer = Client__LSP.make
}
