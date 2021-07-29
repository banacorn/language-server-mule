open NodeJs.Net

// see if the TCP port is available
let probe = (port, host) => {
  let (promise, resolve) = Promise.pending()
  // connect and resolve `Ok()` on success
  let socket = NodeJs.Net.TcpSocket.make()

  socket
  ->NodeJs.Net.TcpSocket.connect(~port, ~host, () => ())
  ->NodeJs.Net.Socket.onConnectOnce(() => resolve(Ok()))
  ->NodeJs.Net.Socket.onErrorOnce(exn => resolve(Error(exn)))
  ->ignore

  // destroy the connection afterwards
  promise->Promise.tap(_ => {
    Socket.destroy(socket, ~error=None)->ignore
  })
}
