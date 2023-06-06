open Tree
open Plot
open Test
open Benchmark
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running




[<EntryPoint>]
let main args =


    // check subTreeConsistencyTest

    let ts =
        Node('a', [ Node('a', [ Node('a', []) ]); Node('a', [ Node('a', []); Node('a', []) ]) ])

    // ts |> plot

    // subTreeConsistencyTest ts

    // check fitTest
    // check symmetryTest
    // check mirrorTest
    // check subTreeConsistencyTest

    let _ = BenchmarkRunner.Run<Benchmarks>()
    0
