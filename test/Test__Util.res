// open Mocha

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

// module Path = {
//   let toAbsolute = filepath => {

//     let dirname = NodeJs.Global.dirname
//     if dirname == "" {
//       NodeJs.Process.cwd(NodeJs.Process.process)
//     } else {
//       NodeJs.Path.resolve([dirname, filepath])
//     }
//   }

//   // replacement of ExtensionContext.getExtensionPath as ExtensionContext.t is out of reach
//   let extensionPath = () => {

//     let dirname = NodeJs.Global.dirname
//     if dirname == "" {
//       NodeJs.Process.cwd(NodeJs.Process.process)
//     } else {
//       NodeJs.Path.resolve([dirname, "../../../../"])
//     }
//   }

//   let asset = filepath => NodeJs.Path.join([extensionPath(), "test/tests/assets", filepath])
// }

// let wait = ms => Promise.make((resolve, _) => Js.Global.setTimeout(resolve, ms)->ignore)

// external jsExnToExn: Js.Exn.t => exn = "%identity"

// module Q = {
//   let toPromise = async f =>
//       switch await f {
//           | Error(error) => Promise.reject(. jsExnToExn(Js.Exn.raiseError(error)))
//           | Ok(result) => Promise.resolve(. result)
//       }

//   let it = (s, f: unit => Promise.t<result<'a, string>>) =>
//     BsMocha.Promise.it(s, () => f()->toPromise)

//   let it_only = (s, f) => BsMocha.Promise.it_only(s, () => f()->toPromise)

//   let it_skip = (s, f) => BsMocha.Promise.it_skip(s, () => f()->toPromise)

//   let before = f => BsMocha.Promise.before(() => f()->toPromise)
//   let before_each = f => BsMocha.Promise.before_each(() => f()->toPromise)
//   let after = f => BsMocha.Promise.after(() => f()->toPromise)
//   let after_each = f => BsMocha.Promise.after_each(() => f()->toPromise)
// }

// module A = {
//   let equal = (expected, actual) =>
//     switch BsMocha.Assert.equal(actual, expected) {
//     | () => Ok()
//     | exception exn => Error(exn)
//     }->Promise.resolved

//   let deep_equal = (expected, actual) =>
//     switch BsMocha.Assert.deep_equal(actual, expected) {
//     | () => Ok()
//     | exception exn => Error(exn)
//     }->Promise.resolved

//   let deep_strict_equal = (expected, actual) =>
//     switch BsMocha.Assert.deep_strict_equal(actual, expected) {
//     | () => Ok()
//     | exception exn => Error(exn)
//     }->Promise.resolved
// }

// module Strings = {
//   // trim and replace all occurences of line breaks with "\n"
//   let normalize = string => {
//     open Js.String
//     replaceByRe(%re("/\\r\\n|\\r/g"), "\n", trim(string))
//   }

//   let serialize = xs => Js.Array.joinWith("\n", xs)

//   let serializeWith = (f, xs) => xs->Array.map(f)->serialize

//   let breakInput = (input: string, breakpoints: array<int>) => {
//     let breakpoints' = Array.concat([0], breakpoints)

//     breakpoints'
//     ->Array.mapWithIndex((i, x: int) =>
//       switch breakpoints'[i + 1] {
//       | Some(next) => (x, next - x)
//       | None => (x, Js.String.length(input) - x)
//       }
//     )
//     ->Array.map(((from, length)) => Js.String.substrAtMost(~from, ~length, input))
//   }
// }
