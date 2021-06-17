open Belt

module Path = {
  let toAbsolute = filepath => {
    let dirname: option<string> = %bs.node(__dirname)
    switch dirname {
    | None => Node.Process.cwd()
    | Some(dirname) => Node.Path.resolve(dirname, filepath)
    }
  }

  // replacement of ExtensionContext.getExtensionPath as ExtensionContext.t is out of reach
  let extensionPath = () => {
    let dirname: option<string> = %bs.node(__dirname)
    switch dirname {
    | None => Node.Process.cwd()
    | Some(dirname) => Node.Path.resolve(dirname, "../../../../")
    }
  }

  let asset = filepath => Node.Path.join([extensionPath(), "test/tests/assets", filepath])
}

let wait = ms => {
  let (promise, resolve) = Promise.pending()
  Js.Global.setTimeout(resolve, ms)->ignore
  promise
}

exception JsExn(Js.Exn.t)

module Q = {
  let toPromise = f =>
    Js.Promise.make((~resolve, ~reject) =>
      f->Promise.get(x =>
        switch x {
        | Error(error) => reject(. JsExn(error))
        | Ok(result) => resolve(. result)
        }
      )
    )

  let it = (s, f: unit => Promise.t<result<'a, Js.Exn.t>>) =>
    BsMocha.Promise.it(s, () => f()->toPromise)

  let it_only = (s, f) => BsMocha.Promise.it_only(s, () => f()->toPromise)

  let it_skip = (s, f) => BsMocha.Promise.it_skip(s, () => f()->toPromise)

  let before = f => BsMocha.Promise.before(() => f()->toPromise)
  let before_each = f => BsMocha.Promise.before_each(() => f()->toPromise)
  let after = f => BsMocha.Promise.after(() => f()->toPromise)
  let after_each = f => BsMocha.Promise.after_each(() => f()->toPromise)
}

module A = {
  let equal = (expected, actual) =>
    switch BsMocha.Assert.equal(actual, expected) {
    | () => Ok()
    | exception exn => Error(exn)
    }->Promise.resolved

  let deep_equal = (expected, actual) =>
    switch BsMocha.Assert.deep_equal(actual, expected) {
    | () => Ok()
    | exception exn => Error(exn)
    }->Promise.resolved

  let deep_strict_equal = (expected, actual) =>
    switch BsMocha.Assert.deep_strict_equal(actual, expected) {
    | () => Ok()
    | exception exn => Error(exn)
    }->Promise.resolved
}

module Strings = {
  // trim and replace all occurences of line breaks with "\n"
  let normalize = string => {
    open Js.String
    replaceByRe(%re("/\\r\\n|\\r/g"), "\n", trim(string))
  }

  let serialize = xs => Js.Array.joinWith("\n", xs)

  let serializeWith = (f, xs) => xs->Array.map(f)->serialize

  let breakInput = (input: string, breakpoints: array<int>) => {
    let breakpoints' = Array.concat([0], breakpoints)

    breakpoints'
    ->Array.mapWithIndex((i, x: int) =>
      switch breakpoints'[i + 1] {
      | Some(next) => (x, next - x)
      | None => (x, Js.String.length(input) - x)
      }
    )
    ->Array.map(((from, length)) => Js.String.substrAtMost(~from, ~length, input))
  }
}
