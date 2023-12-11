module JsError = {
  let toString = (_e: Js.Exn.t): string => %raw("_e.toString()")
}

module Promise = {
  let catch = async f =>
    switch await f() {
    | result => Ok(result)
    | exception Js.Exn.Error(e) => Error(e)
    }
}
