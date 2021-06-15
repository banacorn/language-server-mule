module JsError = {
  let toString = (_e: Js.Exn.t): string => %raw("_e.toString()")
}