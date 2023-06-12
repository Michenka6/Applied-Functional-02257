module Benchmark

open System
open System.Collections.Generic
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open Tree

type Benchmarks() =

    let d = Node("D", [])
    let c = Node("C", [ d; d ])
    let b = Node("B", [ c; c; c ])
    let a = Node("A", [ b; b; b; b ])

    let f = Node("F", [ a; a; a; a; a ])
    let g = Node("G", [ f; f; f; f ])
    let t = Node("T", [ g ])

    let p = design t

    [<Benchmark>]
    member _.moveTreeBTen() =
        let mutable pTree = Node(("", 0.0), [])

        for i = 0 to 10 do
            pTree <- moveTree (p, (float i))

        pTree

    [<Benchmark>]
    member _.moveTreeBTenK() =
        let mutable pTree = Node(("", 0.0), [])

        for i = 0 to 10000 do
            pTree <- moveTree (p, (float i))

        pTree

    [<Benchmark>]
    member _.moveTreeM() =
        let mutable pTree = Node(("", 0.0), [])

        for i = 0 to 1_000_000 do
            pTree <- moveTree (p, (float i))

        pTree

    [<Benchmark>]
    member _.moveTree100M() =
        let mutable pTree = Node(("", 0.0), [])

        for i = 0 to 100_000_000 do
            pTree <- moveTree (p, (float i))

        pTree
