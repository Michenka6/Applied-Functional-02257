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

(*     let t = Node('Z',
        [Node
            (':',
            [Node
                ('E', [Node ('R', [Node ('\004', [Node ('K', [])]); Node ('3', [])])])]);
            Node ('i', [Node ('/', [Node ('n', [])])]); Node ('D', []);
            Node
            ('s',
            [Node ('~', []);
                Node
                ('-', [Node ('m', []); Node ('H', [Node ('\018', [Node ('G', [])])])]);
                Node ('\b', []); Node ('7', []); Node ('\018', []); Node ('A', []);
                Node ('\028', []); Node ('K', []); Node ('5', []); Node ('\018', []);
                Node
                ('"',
                [Node
                    ('\012',
                    [Node ('n', []);
                        Node
                        ('\029', [Node ('<', [Node ('\025', [])]); Node ('\023', [])]);
                        Node
                        ('x',
                        [Node ('u', [Node ('/', []); Node ('{', [])]); Node ('P', [])]);
                        Node ('\'', [Node ('\011', [Node ('\025', [])])])])]);
                Node ('\001', []);
                Node
                ('\000',
                [Node ('}', []);
                    Node
                    ('Z',
                    [Node ('Q', [Node ('J', [Node ('\006', [])])]); Node ('w', [])]);
                    Node ('z', [Node ('n', [])]); Node ('4', []);
                    Node ('\025', [Node ('2', [Node ('\021', [])])])]);
                Node ('I', [Node ('\028', [Node ('\021', [])])]); Node ('w', [])]);
            Node ('N', []); Node ('}', []); Node ('X', [])])

    plot t  *)
    0
