open Tree
open Plot
open Test
open Benchmark
// open BenchmarkDotNet.Attributes
// open BenchmarkDotNet.Running

[<EntryPoint>]
let main args =

    let d = Node("D", [])
    let c = Node("C", [ d; d ])
    let b = Node("B", [ c; c; c ])
    let a = Node("A", [ b; b; b; b ])
    let t = Node("T", [ a ])
    let c2 = Node("1", [d; Node("5", [Node("9", [])])])
    let t2 = Node(
        "0", 
            [Node("3", [c; Node("8", [])]); c2; Node("3", [Node("8", []); c])]
    )

    // check subTreeConsistencyTest

    let ts =
        Node('a', [ Node('a', [ Node('a', []) ]); Node('a', [ Node('a', []); Node('a', []) ]) ])
 
(*     check fitTest
    check symmetryTest
    check mirrorTest
    check subTreeConsistencyTest
    check subTreeConsistencyEncode *)
    check fitProperty
    check symmetryProperty
    check mirrorProperty
    check subTreeConsistencyPropertyEncode
    check treeClassify 
     
    
    //let _ = BenchmarkRunner.Run<Benchmarks>() 
    0
