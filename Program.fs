open Tree
open TreeDrawing
open Test
open Benchmark
// open BenchmarkDotNet.Attributes
// open BenchmarkDotNet.Running

[<EntryPoint>]
let main args =
(*     check fitProperty
    check symmetryProperty
    check mirrorProperty
    check subTreeConsistencyProperty
    check treeClassify *)

    let t1 = Node("Lorem ipsum dolor sit amet, consectetur adipiscing elit", 
        [Node("Lorem ipsum dolor sit amet, consectetur adipiscing elit", [Node("Lorem ipsum dolor sit amet, consectetur adipiscing elit", []);Node("Lorem ipsum dolor sit amet, consectetur adipiscing elit", [Node("Lorem ipsum dolor sit amet, consectetur adipiscing elit", [Node("Lorem ipsum dolor sit amet, consectetur adipiscing elit", [])])]);Node("Lorem ipsum dolor sit amet, consectetur adipiscing elit", [])]); 
        Node("Lorem ipsum dolor sit amet, consectetur adipiscing elit", [])])

    let t2 = Node("ratoootaattoo", 
        [Node("asdjlaksj", [Node("asjdla", [Node("lskadja i wish i wan the way iyam", []); Node("dsjal", [Node("das", [])])])]);
        Node("laras", [Node("lskjadoia", [t1]); Node("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.", []); Node("djalk", [Node("djaskl", []); Node("222daassdsad", [])])])])

    let ast = Node("+", 
        [Node("4", []); Node("+", 
            [Node("*", 
                [Node("2", []); Node("10", [])]);
                Node("*", 
                    [Node("3", []); Node("+", 
                    [Node("5", []); Node("1", [])])])])])

    
    let t4 = Node('\017',
        [Node ('{', []); Node ('\014', []); Node ('o', []); Node ('d', []);
            Node ('\019', []); Node ('n', []); Node ('\029', [Node ('J', [])]);
            Node ('x', []); Node ('\'', [Node ('t', [])]); Node ('\002', []);
            Node ('1', [Node ('t', [Node ('\014', [])])]); Node ('\012', []);
            Node (';', []);
            Node
            ('\022',
            [Node
                ('-', [Node ('\019', [Node ('r', [Node ('O', [Node ('x', [])])])])]);
                Node ('\b', []); Node ('7', [Node ('\019', [])]); Node ('\018', []);
                Node ('A', [Node ('b', [])]); Node ('\028', []);
                Node ('K', [Node ('b', [])]); Node ('&', []);
                Node ('+', [Node ('\012', [])]); Node ('Z', []);
                Node ('5', [Node ('[', [Node ('\031', [])])]); Node ('d', []);
                Node
                ('?',
                [Node
                    ('[',
                    [Node ('\001', [Node (']', [Node ('q', [Node ('N', [])])])])])]);
                Node ('n', []); Node ('I', [Node ('[', [Node ('\022', [])])]);
                Node ('x', []); Node ('S', [Node ('\005', [])]); Node ('\002', []);
                Node (']', [Node ('T', [])]); Node ('\012', []); Node ('g', []);
                Node ('\022', [Node ('>', [])]); Node ('q', []);
                Node (' ', [Node ('\020', [])]); Node ('{', []);
                Node ('*', [Node ('\020', [Node ('g', [])])]); Node ('\005', []);
                Node ('4', [Node ('7', [])]); Node ('\015', []);
                Node ('>', [Node ('y', [])]); Node ('\025', []);
                Node
                ('9',
                [Node ('7', []); Node ('a', []);
                    Node
                    ('>',
                    [Node ('R', []); Node ('7', []); Node ('\013', []);
                        Node ('@', []); Node ('F', []); Node ('\002', [])]);
                    Node
                    ('\020',
                    [Node ('#', []); Node ('\030', [Node ('w', [])]); Node ('D', []);
                        Node
                        ('?',
                        [Node
                            ('\006',
                            [Node ('\b', [Node ('<', []); Node ('\029', [])])]);
                            Node ('\019', [])])]); Node ('<', []);
                    Node ('\009', [Node ('d', [Node ('8', [])])])]);
                Node ('J', [Node ('H', [Node (',', [])])]); Node ('\025', []);
                Node
                (' ',
                [Node
                    ('8',
                    [Node
                        ('e',
                        [Node ('\010', []);
                            Node ('9', [Node ('a', [Node ('E', [])])]);
                            Node ('\020', [])]); Node ('i', [])])]); Node ('{', []);
                Node ('X', [Node ('f', [Node ('=', [])])]); Node ('w', []);
                Node ('^', []); Node ('.', [Node ('\024', [])]); Node ('S', []);
                Node ('\029', []); Node ('v', [Node ('\003', [Node ('x', [])])]);
                Node ('k', [Node ('A', [])]); Node ('\026', []);
                Node
                ('u',
                [Node
                    ('%', [Node ('f', [Node ('s', [Node ('/', [Node (',', [])])])])])]);
                Node ('$', []); Node ('m', []); Node ('T', []);
                Node
                ('<',
                [Node
                    ('c',
                    [Node ('"', [Node ('\021', [Node ('\\', [Node ('\\', [])])])])])]);
                Node ('_', [Node ('^', [Node ('\028', [])])]);
                Node ('O', [Node ('J', [])]);
                Node ('K', [Node ('z', [Node ('L', [Node ('%', [Node ('v', [])])])])]);
                Node ('{', [Node ('=', [])]); Node ('p', []);
                Node ('\031', [Node ('\b', [Node ('v', [])])]); Node ('z', []);
                Node (')', [Node ('W', [Node ('5', [])])]); Node ('.', []);
                Node ('\009', []); Node ('8', []); Node ('\019', [Node ('o', [])]);
                Node ('B', []); Node ('\029', []);
                Node ('\007', [Node ('x', [Node ('\020', [])])]); Node ('K', []);
                Node ('R', []);
                Node ('*', [Node ('U', [Node ('g', [Node ('>', [Node ('s', [])])])])]);
                Node ('s', [Node ('w', [])]); Node ('Z', [])]); Node ('E', []);
            Node (' ', []); Node ('O', []); Node ('*', []); Node ('Y', []);
            Node ('4', []); Node ('9', []); Node ('h', []); Node ('C', []);
            Node ('r', []); Node ('M', []); Node ('|', []); Node ('W', []);
            Node ('\006', []); Node ('a', []); Node ('\016', []); Node ('k', []);
            Node ('\026', [Node ('u', [Node ('\004', [])])]); Node ('u', []);
            Node ('$', [Node ('u', [])]); Node ('\127', []); Node ('.', []);
            Node ('\009', []); Node ('8', []); Node ('\019', []); Node ('B', [])])

    let t3 = Node('\b',
        [Node ('H', []); Node ('.', []); Node ('k', []); Node ('\021', []);
            Node ('\027', []); Node ('W', []); Node (' ', []); Node ('{', []);
            Node ('*', []); Node ('\005', []); Node ('4', []); Node ('\015', []);
            Node ('>', []); Node ('\025', []); Node ('H', []); Node ('F', []);
            Node ('!', []); Node ('P', []); Node ('+', []); Node ('Z', []);
            Node ('5', []); Node ('d', []);
            Node
            ('?',
            [Node ('d', [Node ('+', [Node ('\011', [Node ('x', [])])])]);
                Node ('\019', []);
                Node ('n', [Node ('U', [Node (' ', [Node ('\018', [])])])]);
                Node ('\029', []); Node ('x', [Node ('U', [])]); Node ('\'', [])]);
            Node ('n', []); Node ('I', []); Node ('x', []); Node ('S', []);
            Node ('\002', []); Node (']', []); Node ('\012', []); Node ('g', []);
            Node ('\022', []); Node ('q', []); Node (' ', []); Node ('{', []);
            Node ('*', []); Node ('\005', []); Node ('4', []); Node ('\015', []);
            Node ('>', []); Node ('\025', []); Node ('H', []); Node ('M', []);
            Node ('(', []); Node ('W', []); Node ('2', []); Node ('a', []);
            Node ('<', []);
            Node
            ('k',
            [Node ('r', []);
                Node ('M', [Node ('{', [Node ('1', [Node ('B', [Node ('5', [])])])])]);
                Node ('|', []); Node ('W', [Node ('\029', [])]);
                Node ('\006', [Node ('e', [])]);
                Node
                ('V',
                [Node ('i', []);
                    Node
                    ('9',
                    [Node ('\005', []); Node ('\018', []);
                        Node
                        ('6',
                        [Node
                            ('_',
                            [Node ('\015', []);
                                Node
                                ('\010',
                                [Node ('\010', []); Node ('*', []); Node ('A', [])])])])]);
                    Node ('m', []);
                    Node
                    ('@',
                    [Node ('3', [Node ('\031', [Node (':', [Node ('\023', [])])])]);
                        Node ('a', []); Node ('U', [])]); Node ('\029', [])]);
                Node
                ('J', [Node (';', [Node ('w', [Node ('O', [Node ('\004', [])])])])]);
                Node ('&', []); Node ('5', []); Node ('\012', []); Node ('@', []);
                Node ('2', [Node ('\004', [])]); Node ('x', []); Node ('\015', []);
                Node ('n', []); Node ('\020', []); Node ('\018', []);
                Node
                ('^',
                [Node
                    (')',
                    [Node ('\029', [Node ('>', [Node ('/', [Node ('j', [t4])])])])])]);
                Node ('4', [Node ('\021', [Node ('\025', []); Node ('\007', [])])]);
                Node ('\017', []); Node ('8', []); Node ('i', []); Node ('\029', []);
                Node ('\\', [Node ('*', [])]); Node ('\023', []); Node ('u', []);
                Node ('`', [Node ('g', [Node ('N', [])])]); Node ('E', [Node ('o', [])]);
                Node ('\014', []);
                Node
                ('i',
                [Node ('o', [Node ('5', [Node ('}', [Node ('\012', [])])])]);
                    Node ('\030', [])]); Node ('\024', []); Node ('s', []);
                Node ('"', [Node ('/', [Node ('f', [])])]); Node ('}', []);
                Node (',', [Node ('/', [])]); Node ('\007', []);
                Node
                ('6',
                [Node ('R', []);
                    Node
                    ('-', [Node ('G', [Node ('-', [Node ('b', [Node ('?', [])])])])]);
                    Node ('\\', []);
                    Node
                    ('7',
                    [Node
                        ('G', [Node ('\022', [Node ('\031', [Node ('\003', [])])])])]);
                    Node ('f', []); Node ('A', [Node ('\029', [Node ('$', [])])]);
                    Node ('p', []); Node ('K', [Node ('\029', [])]); Node ('z', []);
                    Node ('U', [Node ('@', [])]); Node ('\004', []);
                    Node ('_', [Node ('@', [])]); Node ('\014', []);
                    Node ('i', [Node ('\022', [])]); Node ('\024', []);
                    Node ('s', [Node ('\022', [])]); Node ('"', []);
                    Node ('}', [Node ('r', [])]); Node (',', [])]); Node ('\017', []);
                Node ('@', [Node ('R', [])]); Node ('\027', []);
                Node ('J', [Node ('(', [])]); Node ('%', []); Node ('T', []);
                Node ('R', []); Node ('-', [Node ('K', [Node ('|', [])])]);
                Node ('\\', []); Node ('7', [Node ('K', [Node ('\010', [])])]);
                Node ('f', []);
                Node ('A', [Node ('!', [Node ('I', [Node ('\031', [])])])]);
                Node ('p', []); Node ('K', [Node ('!', [])]); Node ('z', []);
                Node ('U', [Node ('D', [])]); Node ('\004', []);
                Node ('_', [Node ('D', [])]); Node ('\014', []);
                Node ('i', [Node ('\021', [])]); Node ('\024', [Node ('o', [])]);
                Node ('\018', [Node ('\005', [])]);
                Node ('\012', [Node ('W', [Node ('l', [])])]); Node ('\000', [])]);
            Node ('F', []); Node ('u', []); Node ('P', []); Node ('\127', []);
            Node ('Z', []); Node ('\009', []); Node ('d', []); Node ('+', []);
            Node ('@', []);
            Node
            ('7',
            [Node ('\014', []); Node ('B', []); Node ('p', []);
                Node
                ('n',
                [Node
                    ('M',
                    [Node ('\028', [Node ('U', [])]);
                        Node ('\025', [Node ('K', [Node ('\017', [Node ('u', [t4])])])])])]);
                Node
                ('\029',
                [Node ('\010', [Node ('I', [Node ('\009', [])])]); Node ('\021', [])]);
                Node ('\b', [Node ('r', [])]); Node ('T', []);
                Node
                ('3',
                [Node
                    ('\b',
                    [Node
                        ('M',
                        [Node ('A', []);
                            Node ('*', [Node ('\024', [Node ('u', [])])])])])])]);
            Node ('c', []); Node ('j', []); Node ('u', []); Node ('Z', []);
            Node ('\001', []); Node ('r', []); Node ('`', []); Node ('$', []);
            Node ('\127', []); Node ('&', []); Node ('k', []); Node ('\021', []);
            Node ('\026', []); Node ('\004', []); Node ('Z', []); Node ('7', []);
            Node ('a', []); Node ('"', []); Node ('\021', []); Node ('\025', [])])



    TreeDrawing.generateDrawing(t3, firstn=1, hover=true, fontSize=9.0) |> saveDrawing "laaaargeTree.html"

    0
