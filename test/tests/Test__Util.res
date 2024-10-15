// Like `Promise.make` but without having to supply a callback
let pending: unit => (promise<'a>, 'a => unit, 'e => unit) = () => {
  let resolve = ref(None)
  let reject = ref(None)
  let promise = Promise.make((res, rej) => {
    resolve := Some(res)
    reject := Some(rej)
  })

  switch (resolve.contents, reject.contents) {
  | (Some(resolve), Some(reject)) => (promise, resolve, reject)
  | _ => raise(Failure("Promise is not initialized"))
  }
}

module Platform = {
  module GetOs = {
    type t = {"os": string, "dist": string, "codename": string, "release": string}

    @module
    external getos: (('e, t) => unit) => unit = "getos"

    let run = (): promise<t> => {
      let (promise, resolve, reject) = pending()
      getos((e, os) => {
        let e = Js.Nullable.toOption(e)
        switch e {
        | Some(e) => reject(e)
        | None => resolve(os)
        }
      })
      promise
    }
  }

  type t = Windows | MacOS | Ubuntu | Others(string, GetOs.t)

  let determine = async () =>
    switch NodeJs.Os.platform() {
    | "darwin" => Ok(MacOS)
    | "linux" =>
      // determine the distro
      switch await GetOs.run() {
      | info =>
        switch info["dist"] {
        | "Ubuntu" => Ok(Ubuntu)
        | _ => Ok(Others("linux", info))
        }
      | exception Exn.Error(e) => Error(e)
      }
    | "win32" => Ok(Windows)
    | others =>
      // determine the distro
      switch await GetOs.run() {
      | info => Ok(Others(others, info))
      | exception Exn.Error(e) => Error(e)
      }
    }
}
