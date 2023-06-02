open Tree
open Plot
open Test

let d = Node("D", [])
let c = Node("C", [ d; d ])
let b = Node("B", [ c; c; c ])
let a = Node("A", [ b; b; b; b ])
let t = Node("T", [ a ])
// let t = Node("T", [ c; c; d ])

let t1 =
    Node(
        '#',
        [ Node('\'', [])
          Node('V', [])
          Node('1', [])
          Node('`', [])
          Node(';', [ Node(' ', []); Node('O', []); Node('*', [ Node('R', []) ]) ])
          Node('j', []) ]
    )




[<EntryPoint>]
let main args =
    // plot t1

    check symmetryTest
    0
