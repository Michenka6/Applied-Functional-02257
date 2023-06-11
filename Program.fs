open Tree
open TreeDrawing
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

    let t4 = Node("Lorem ipsum dolor sit amet, consectetur adipiscing elit", 
        [Node("Lorem ipsum dolor sit amet, consectetur adipiscing elit", [Node("Lorem ipsum dolor sit amet, consectetur adipiscing elit", []);Node("Lorem ipsum dolor sit amet, consectetur adipiscing elit", [Node("Lorem ipsum dolor sit amet, consectetur adipiscing elit", [Node("Lorem ipsum dolor sit amet, consectetur adipiscing elit", [])])]);Node("Lorem ipsum dolor sit amet, consectetur adipiscing elit", [])]); 
        Node("Lorem ipsum dolor sit amet, consectetur adipiscing elit", [])])

    let t5 = Node("ratoootaattoo", 
        [Node("asdjlaksj", [Node("asjdla", [Node("lskadja i wish i wan the way iyam", []); Node("dsjal", [Node("das", [])])])]);
        Node("laras", [Node("lskjadoia", [t4]); Node("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.", []); Node("djalk", [Node("djaskl", []); Node("222daassdsad", [])])])])

    TreeDrawing.generateDrawing(t5, scale=5.0, firstn=11, hover=true, fontSize=12.0) |> showDrawing 
    
    0
