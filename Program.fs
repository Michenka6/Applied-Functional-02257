open Tree
open Plot
open Test

let d = Node("D", [])
let c = Node("C", [ d; d ])
let b = Node("B", [ c; c; c ])
let a = Node("A", [ b; b; b; b ])
let t = Node("T", [ a ])
// let t = Node("T", [ c; c; d ])


[<EntryPoint>]
let main args =
    // plot t

    check fitTest
    0
