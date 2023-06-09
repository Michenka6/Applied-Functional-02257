open Tree
open Plot
open Test
open Benchmark
// open BenchmarkDotNet.Attributes
// open BenchmarkDotNet.Running

[<EntryPoint>]
let main args =
    check fitProperty
    check symmetryProperty
    check mirrorProperty
    check subTreeConsistencyProperty
    check treeClassify
    //let _ = BenchmarkRunner.Run<Benchmarks>() 
    0
