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

    let t = Node('M',
        [Node
            ('5',
            [Node
                ('\029',
                [Node ('W', []); Node ('\006', []);
                    Node
                    ('a',
                    [Node
                        ('.',
                        [Node ('2', []);
                            Node
                            ('a',
                            [Node ('q', []); Node ('d', []); Node (',', [])])])])])]);
            Node ('d', []); Node ('?', []);
            Node
            ('n',
            [Node ('3', []);
                Node
                ('b',
                [Node ('r', []); Node ('w', []); Node ('&', []); Node ('z', []);
                    Node ('0', []); Node ('\011', []); Node (':', []); Node ('\021', []);
                    Node
                    ('D',
                    [Node
                        ('*',
                        [Node ('?', []); Node ('\026', []);
                            Node ('I', [Node ('&', []); Node ('r', []); Node ('7', [])])]);
                        Node
                        ('\005',
                        [Node ('x', []); Node ('S', []); Node ('s', []);
                            Node (']', [Node ('k', [])]); Node ('c', []);
                            Node ('g', []); Node ('\022', []);
                            Node ('q', [Node ('U', [])]); Node (' ', []);
                            Node ('{', [Node ('x', [])]); Node ('*', []);
                            Node ('Q', [Node ('s', []); Node ('G', [])]);
                            Node ('f', [])]); Node ('4', [Node ('a', [])])]);
                    Node ('B', []); Node ('\029', []); Node ('L', []); Node ('\'', []);
                    Node ('V', []); Node ('1', []); Node ('`', []); Node (';', []);
                    Node ('j', []); Node ('E', [])])]); Node ('I', []); Node ('x', []);
            Node ('S', []); Node ('g', []); Node (']', []); Node ('b', []);
            Node
            ('v',
            [Node ('\022', []); Node ('q', []); Node (' ', []);
                Node
                ('{',
                [Node
                    ('D',
                    [Node ('S', [Node ('m', [])]); Node ('\002', []); Node (']', []);
                        Node ('j', []); Node ('g', [])])]); Node ('*', []);
                Node ('l', [])])])    

    TreeDrawing.generateDrawing(t, firstn=1, hover=true, fontSize=10.0) |> saveDrawing "mediumSizedTree.html"

    0
