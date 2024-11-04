module JsError = {
  let toString = (_e: Js.Exn.t): string => %raw("_e.toString()")
}

module Promise = {
  let catch = async f =>
    switch await f() {
    | result => Ok(result)
    | exception Js.Exn.Error(e) => Error(e)
    }

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

  let setTimeout = async time => {
    let (promise, resolve, _) = pending()
    let id = Js.Global.setTimeout(resolve, time)
    await promise
    Js.Global.clearTimeout(id)
  }
}
