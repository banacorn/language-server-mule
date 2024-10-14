// Like `Promise.make` but without having to supply a callback
let pending: unit => (promise<'a>, unit => unit, 'e => unit) = () => {
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


// module Platform = {
//   module GetOs = {
//     type t = {"os": string, "dist": string, "codename": string, "release": string}

//     @module
//     external getos: (('e, t) => unit) => unit = "getos"

//     let runAsPromise = (): promise<t> => {
//       let (promise, resolve, reject) = Promise.Js.pending()
//       getos((e, os) => {
//         let e = Js.Nullable.toOption(e)
//         switch e {
//         | Some(e) => reject(e)
//         | None => resolve(os)
//         }
//       })
//       promise
//     }
//   }

//   type t = Windows | MacOS | Ubuntu | Others

//   let determine = () =>
//     switch Node_process.process["platform"] {
//     | "darwin" => Promise.resolved(MacOS)
//     | "linux" =>
//       // determine the distro
//       GetOs.runAsPromise()->Promise.map(result =>
//         switch result["dist"] {
//         | "Ubuntu" => Ubuntu
//         | _ => Others
//         }
//       )
//     | "win32" => Promise.resolved(Windows)
//     | _others => Promise.resolved(Others)
//     }
// }
