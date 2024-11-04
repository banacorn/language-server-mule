open NodeJs.Net

// see if the TCP port is available
let probe = (port, host, ~timeout=1000) => {
  let timeout = async () => {
    await Util.Promise.setTimeout(timeout)
    Error(Js.Exn.raiseError("timeout"))
  }

  let connection = Promise.make((resolve, _) => {
    // connect and resolve `Ok()` on success
    let socket = NodeJs.Net.TcpSocket.make()

    socket
    ->NodeJs.Net.TcpSocket.connect(~port, ~host, () => ())
    ->NodeJs.Net.Socket.onConnectOnce(() => {
      // destroy the connection afterwards
      Socket.destroy(socket, ~error=None)->ignore
      resolve(Ok())
    })
    ->NodeJs.Net.Socket.onErrorOnce(exn => resolve(Error(exn)))
    ->NodeJs.Net.Socket.onTimeoutOnce(() => resolve(Error(Js.Exn.raiseError("timeout"))))
    ->ignore
  })

  Promise.race([connection, timeout()])
}
