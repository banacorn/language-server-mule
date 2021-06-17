open BsMocha.Mocha
module Assert = BsMocha.Assert

open Test__Util

describe("Path Searching", () => {
  Q.it("Command provided by `whichCommand` should exist", () => {
    switch Search__Path.whichCommand {
    | Error(os) => Promise.resolved(Error(Js.Exn.raiseError(os)))
    | Ok(command) =>
      let (promise, resolve) = Promise.pending()
      NodeJs.ChildProcess.exec(command, (error, stdout, stderr) => {
        switch Js.Nullable.toOption(error) {
        | Some(exn) => resolve(Error(exn))
        | None =>
          if NodeJs.Buffer.toString(stderr) == "" {
            resolve(Ok(NodeJs.Buffer.toString(stdout)))
          } else {
            resolve(Error(Js.Exn.raiseError(NodeJs.Buffer.toString(stdout))))
          }
        }
      })->ignore
      promise
    }

    //
  })
  // Q.it("should calculate the infomation needed for case splitting correctly", () =>
  //   VSCode.Window.showTextDocumentWithUri(
  //     VSCode.Uri.file(Path.asset("CaseSplit.agda")),
  //     None,
  //   )->Promise.flatMap(editor => {
  //     let document = VSCode.TextEditor.document(editor)
  //     Goal.makeMany(editor, [0, 1, 2, 3, 4, 5, 6, 7, 8])
  //     ->Promise.map(goals =>
  //       goals->Array.map(goal => {
  //         // convert `rewriteRange` to text in that range because range offsets are different on different OSs
  //         let (inWhereClause, indentWidth, rewriteRange) = State__Goal.caseSplitAux(document, goal)
  //         let rewriteRange = VSCode.Range.make(
  //           VSCode.TextDocument.positionAt(document, fst(rewriteRange)),
  //           VSCode.TextDocument.positionAt(document, snd(rewriteRange)),
  //         )
  //         (inWhereClause, indentWidth, Editor.Text.get(document, rewriteRange))
  //       })
  //     )
  //     ->Promise.map(results => Ok(
  //       Assert.deep_equal(
  //         results,
  //         [
  //           (false, 9, j`x → {!   !}`),
  //           (false, 23, j`y → {!   !}`),
  //           (false, 4, j`x → {!   !}`),
  //           (false, 4, j`y → {!   !}`),
  //           (true, 13, j`x → {!   !}`),
  //           (true, 13, j`y → {!   !}`),
  //           (true, 2, j`x → {!   !}`),
  //           (true, 2, j`y → {!   !}`),
  //           (false, 13, j`x → {!   !}`),
  //         ],
  //       ),
  //     ))
  //   })
  // )
})
