open Tree
open Plot
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
    //let _ = BenchmarkRunner.Run<Benchmarks>()

    let ttt = Node('Z',
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
            
    let tttt = Node("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party",
        [Node
            ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party",
            [Node
                ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", [Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", [Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", [Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", [])]); Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", [])])])]);
            Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", [Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", [Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", [])])]); Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", []);
            Node
            ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party",
            [Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", []);
                Node
                ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", [Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", []); Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", [Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", [Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", [])])])]);
                Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", []); Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", []); Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", []); Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", []);
                Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", []); Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", []); Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", []); Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", []);
                Node
                ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party",
                [Node
                    ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party",
                    [Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", []);
                        Node
                        ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", [Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", [Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", [])]); Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", [])]);
                        Node
                        ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party",
                        [Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", [Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", []); Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", [])]); Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", [])]);
                        Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", [Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", [Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", [])])])])]);
                Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", []);
                Node
                ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party",
                [Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", []);
                    Node
                    ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party",
                    [Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", [Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", [Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", [])])]); Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", [])]);
                    Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", [Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", [])]); Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", []);
                    Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", [Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", [Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", [])])])]);
                Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", [Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", [Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", [])])]); Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", [])]);
            Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", []); Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", []); Node ("Hey, yeah, I heard you met my friend the other weekend\n I heard that he was telling croaks at the party", [])])

   
(*     let tt = Node('k',
            [Node ('\025', []); Node ('H', []); Node ('#', []); Node ('R', []);
                Node
                ('-',
                [Node
                    ('~',
                    [Node ('\012', []); Node (';', [Node ('2', [])]); Node ('\022', []);
                        Node
                        ('E',
                        [Node ('\b', []);
                            Node ('7', [Node ('\018', [Node ('h', []); Node ('C', [])])])])]);
                    Node ('Y', []); Node ('\b', []); Node ('c', []); Node ('\018', [])]);
                Node ('\\', []); Node ('7', []);
                Node
                ('f',
                [Node
                    ('7',
                    [Node
                        ('\017',
                        [Node ('/', [Node ('\001', [Node (')', [])])]); Node ('^', []);
                            Node ('9', [Node ('\001', [])]); Node ('h', []);
                            Node ('C', [Node ('P', [])]); Node ('r', []);
                            Node ('M', [Node ('z', [])]); Node ('|', []);
                            Node
                            ('W',
                            [Node
                                ('z',
                                [Node ('\023', []); Node ('F', []);
                                    Node
                                    ('!',
                                    [Node (')', []); Node ('$', []); Node ('*', []);
                                        Node ('G', []); Node ('X', []); Node ('P', [])]);
                                    Node ('C', [])])]); Node ('\006', []); Node ('a', []);
                            Node ('\016', [Node ('3', [Node ('\015', [])])]); Node ('k', []);
                            Node
                            ('\026', [Node ('\009', [Node ('h', [Node ('\027', [])])])]);
                            Node ('u', []); Node ('$', []); Node ('\127', []);
                            Node (';', []); Node ('[', []); Node ('$', [Node ('\024', [])]);
                            Node ('"', []); Node ('}', [Node ('\024', [])]); Node (',', []);
                            Node ('\007', []); Node ('6', []); Node ('\017', []);
                            Node ('@', []); Node ('\027', [Node ('\017', [])]);
                            Node ('J', []); Node ('%', [Node ('\001', [])]); Node ('T', []);
                            Node ('3', []); Node ('f', [Node ('&', [])]); Node ('\029', []);
                            Node ('\020', []); Node ('$', []);
                            Node ('\004', [Node ('%', [Node ('F', [])])])]); Node ('l', []);
                        Node ('\027', []); Node ('v', []); Node ('%', []); Node ('\000', []);
                        Node ('/', []); Node ('\010', []); Node ('9', []); Node ('\020', []);
                        Node
                        ('C',
                        [Node
                            ('(',
                            [Node ('.', []);
                                Node ('\009', [Node ('z', [Node ('\020', [])])])]);
                            Node
                            ('W',
                            [Node ('D', [Node ('V', []); Node ('K', [Node ('/', [])])]);
                                Node ('s', [])]); Node ('2', [])])]);
                    Node
                    ('\018',
                    [Node
                        ('\'',
                        [Node ('u', []);
                            Node
                            ('P',
                            [Node ('\014', []);
                                Node ('i', [Node ('"', []); Node ('}', [])])]);
                            Node ('\127', []); Node ('Z', []);
                            Node
                            ('\009', [Node ('\029', [Node ('=', [Node ('\026', [])])])]);
                            Node
                            ('d',
                            [Node
                                ('\007',
                                [Node ('Y', []);
                                    Node
                                    ('4',
                                    [Node ('\024', []); Node ('z', []); Node ('\127', []);
                                        Node (' ', []); Node ('\027', [])])])]);
                            Node ('\019', []); Node ('n', []); Node ('\029', [])]);
                        Node ('V', []); Node ('1', []); Node ('`', []);
                        Node (';', [Node ('u', [])]); Node ('j', [])])]); Node ('A', []);
                Node ('p', []); Node ('K', []);
                Node
                ('z',
                [Node ('\013', []); Node ('<', []); Node ('\023', []);
                    Node ('F', [Node ('\030', [Node ('a', [])])]); Node ('!', []);
                    Node ('P', []); Node ('+', []); Node ('Z', []); Node ('5', []);
                    Node ('d', []); Node ('?', []);
                    Node
                    ('n',
                    [Node ('\023', []); Node ('F', []); Node ('!', []); Node ('P', []);
                        Node ('+', []);
                    Node
                        ('4',
                        [Node ('Q', []); Node ('\000', []); Node ('[', []);
                            Node ('\010', []);
                            Node
                            ('e', [Node ('\020', []); Node ('o', []); Node ('\030', [])])]);
                        Node ('\031', []); Node ('h', []); Node ('1', []); Node ('\012', []);
                        Node (';', []); Node ('\022', []);
                        Node
                        ('E',
                        [Node
                            ('f',
                            [Node
                                ('I',
                                [Node ('c', []); Node ('>', [Node ('"', [])]);
                                    Node ('m', [Node ('Q', [])])]);
                                Node ('$', [Node ('#', [Node ('\000', [])])])]);
                            Node ('\021', [])]); Node (' ', []); Node ('O', []);
                        Node ('*', []); Node ('Y', []); Node ('4', []); Node ('c', []);
                        Node ('a', []);
                        Node
                        ('<',
                        [Node
                            ('_',
                            [Node ('7', []); Node ('\018', []);
                                Node ('A', [Node ('\026', [Node ('$', [])])]);
                                Node
                                ('\028',
                                [Node ('\004', []);
                                    Node
                                    ('_', [Node ('9', []); Node ('[', []); Node (' ', [])]);
                                    Node ('\010', []); Node ('\127', [])]);
                                Node ('K', [Node ('=', [])])]); Node ('\014', [])])])]);
                Node ('U', [Node ('w', [])]); Node ('\004', []); Node ('_', []);
                Node ('\014', []); Node ('i', []); Node ('\024', []);
                Node ('s', [Node ('p', [])]); Node ('"', [])]) *)

    let tt = Node("k", [Node ("0025", []); Node ("H", []); Node ("#", []); Node ("R", []);
                Node
                ("-",
                [Node
                    ("~",
                    [Node ("0012", []); Node (";", [Node ("2", [])]); Node ("0022", []);
                        Node
                        ("E",
                        [Node ("0b", []);
                            Node ("7", [Node ("0018", [Node ("h", []); Node ("C", [])])])])]);
                    Node ("Y", []); Node ("0b", []); Node ("c", []); Node ("0018", [])]);
                Node ("00", []); Node ("7", []);
                Node
                ("f",
                [Node
                    ("7",
                    [Node
                        ("0017",
                        [Node ("/", [Node ("0001", [Node (")", [])])]); Node ("^", []);
                            Node ("9", [Node ("0001", [])]); Node ("h", []);
                            Node ("C", [Node ("P", [])]); Node ("r", []);
                            Node ("M", [Node ("z", [])]); Node ("|", []);
                            Node
                            ("W",
                            [Node
                                ("z",
                                [Node ("0023", []); Node ("F", []);
                                    Node
                                    ("!",
                                    [Node (")", []); Node ("$", []); Node ("*", []);
                                        Node ("G", []); Node ("X", []); Node ("P", [])]);
                                    Node ("C", [])])]); Node ("0006", []); Node ("a", []);
                            Node ("0016", [Node ("3", [Node ("0015", [])])]); Node ("k", []);
                            Node
                            ("0026", [Node ("0009", [Node ("h", [Node ("0027", [])])])]);
                            Node ("u", []); Node ("$", []); Node ("0127", []);
                            Node (";", []); Node ("[", []); Node ("$", [Node ("0024", [])]);
                            Node ("lala", []); Node ("}", [Node ("0024", [])]); Node (",", []);
                            Node ("0007", []); Node ("6", []); Node ("0017", []);
                            Node ("@", []); Node ("0027", [Node ("0017", [])]);
                            Node ("J", []); Node ("%", [Node ("0001", [])]); Node ("T", []);
                            Node ("3", []); Node ("f", [Node ("&", [])]); Node ("0029", []);
                            Node ("0020", []); Node ("$", []);
                            Node ("0004", [Node ("%", [Node ("F", [])])])]); Node ("l", []);
                        Node ("0027", []); Node ("v", []); Node ("%", []); Node ("0000", []);
                        Node ("/", []); Node ("0010", []); Node ("9", []); Node ("0020", []);
                        Node
                        ("C",
                        [Node
                            ("(",
                            [Node (".", []);
                                Node ("0009", [Node ("z", [Node ("0020", [])])])]);
                            Node
                            ("W",
                            [Node ("D", [Node ("V", []); Node ("K", [Node ("/", [])])]);
                                Node ("s", [])]); Node ("2", [])])]);
                    Node("0018",
                    [Node
                        ("0",
                        [Node ("u", []);
                            Node
                            ("P",
                            [Node ("0014", []);
                                Node ("i", [Node ("lala", []); Node ("}", [])])]);
                            Node ("0127", []); Node ("Z", []);
                            Node
                            ("0009", [Node ("0029", [Node ("=", [Node ("0026", [])])])]);
                            Node
                            ("d",
                            [Node
                                ("0007",
                                [Node ("Y", []);
                                    Node
                                    ("4",
                                    [Node ("0024", []); Node ("z", []); Node ("0127", []);
                                        Node (" ", []); Node ("0027", [])])])]);
                            Node ("0019", []); Node ("n", []); Node ("0029", [])]);
                        Node ("V", []); Node ("1", []); Node ("`", []);
                        Node (";", [Node ("u", [])]); Node ("j", [])])]); Node ("A", []);
                Node ("p", []); Node ("K", []);
                Node
                ("z",
                [Node ("0013", []); Node ("<", []); Node ("0023", []);
                    Node ("F", [Node ("0030", [Node ("a", [])])]); Node ("!", []);
                    Node ("P", []); Node ("+", []); Node ("Z", []); Node ("5", []);
                    Node ("d", []); Node ("?", []);
                    Node
                    ("n",
                    [Node ("0023", []); Node ("F", []); Node ("!", []); Node ("P", []);
                        Node ("+", []);
                    Node
                        ("4",
                        [Node ("Q", []); Node ("0000", []); Node ("[", []);
                            Node ("0010", []);
                            Node
                            ("e", [Node ("0020", []); Node ("o", []); Node ("0030", [])])]);
                        Node ("0031", []); Node ("h", []); Node ("1", []); Node ("0012", []);
                        Node (";", []); Node ("0022", []);
                        Node
                        ("E",
                        [Node
                            ("f",
                            [Node
                                ("I",
                                [Node ("c", []); Node (">", [Node ("lala", [])]);
                                    Node ("m", [Node ("Q", [])])]);
                                Node ("$", [Node ("#", [Node ("0000", [])])])]);
                            Node ("0021", [])]); Node (" ", []); Node ("O", []);
                        Node ("*", []); Node ("Y", []); Node ("4", []); Node ("c", []);
                        Node ("a", []);
                        Node
                        ("<",
                        [Node
                            ("_",
                            [Node ("7", []); Node ("0018", []);
                                Node ("A", [Node ("0026", [Node ("$", [])])]);
                                Node
                                ("0028",
                                [Node ("0004", []);
                                    Node
                                    ("_", [Node ("9", []); Node ("[", []); Node (" ", [tttt])]);
                                    Node ("0010", []); Node ("0127", [])]);
                                Node ("K", [Node ("=", [])])]); Node ("0014", [])])])]);
                Node ("U", [Node ("w", [])]); Node ("0004", []); Node ("_", []);
                Node ("0014", []); Node ("i", []); Node ("0024", []);
                Node ("s", [Node ("p", [])]); Node ("asda", [])])

    
    let tlong = Node("k", [Node ("0025", []); Node ("H", []); Node ("#", []); Node ("R", []);
            Node
            ("-",
            [Node
                ("~",
                [Node ("0012", []); Node (";", [Node ("2", [])]); Node ("0022", []);
                    Node
                    ("E",
                    [Node ("0b", []);
                        Node ("7", [Node ("0018", [Node ("h", []); Node ("C", [])])])])]);
                Node ("Y", []); Node ("0b", []); Node ("c", []); Node ("0018", [])]);
            Node ("00", []); Node ("7", []);
            Node
            ("f",
            [Node
                ("7",
                [Node
                    ("0017",
                    [Node ("/", [Node ("0001", [Node (")", [])])]); Node ("^", []);
                        Node ("9", [Node ("0001", [])]); Node ("h", []);
                        Node ("C", [Node ("P", [])]); Node ("r", []);
                        Node ("M", [Node ("z", [])]); Node ("|", []);
                        Node
                        ("W",
                        [Node
                            ("z",
                            [Node ("0023", []); Node ("F", []);
                                Node
                                ("!",
                                [Node (")", []); Node ("$", []); Node ("*", []);
                                    Node ("G", []); Node ("X", []); Node ("P", [])]);
                                Node ("C", [])])]); Node ("0006", []); Node ("a", []);
                        Node ("0016", [Node ("3", [Node ("0015", [])])]); Node ("k", []);
                        Node
                        ("0026", [Node ("0009", [Node ("h", [Node ("0027", [])])])]);
                        Node ("u", []); Node ("$", []); Node ("0127", []);
                        Node (";", []); Node ("[", []); Node ("$", [Node ("0024", [])]);
                        Node ("lala", []); Node ("}", [Node ("0024", [])]); Node (",", []);
                        Node ("0007", []); Node ("6", []); Node ("0017", []);
                        Node ("@", []); Node ("0027", [Node ("0017", [])]);
                        Node ("J", []); Node ("%", [Node ("0001", [])]); Node ("T", []);
                        Node ("3", []); Node ("f", [Node ("&", [])]); Node ("0029", []);
                        Node ("0020", []); Node ("$", []);
                        Node ("0004", [Node ("%", [Node ("F", [])])])]); Node ("l", []);
                    Node ("0027", []); Node ("v", []); Node ("%", []); Node ("0000", []);
                    Node ("/", []); Node ("0010", []); Node ("9", []); Node ("0020", []);
                    Node
                    ("C",
                    [Node
                        ("(",
                        [Node (".", []);
                            Node ("0009", [Node ("z", [Node ("0020", [])])])]);
                        Node
                        ("W",
                        [Node ("D", [Node ("V", []); Node ("K", [Node ("/", [])])]);
                            Node ("s", [])]); Node ("2", [])])]);
                Node("0018",
                [Node
                    ("0",
                    [Node ("u", []);
                        Node
                        ("P",
                        [Node ("0014", []);
                            Node ("i", [Node ("lala", []); Node ("}", [])])]);
                        Node ("0127", []); Node ("Z", []);
                        Node
                        ("0009", [Node ("0029", [Node ("=", [Node ("0026", [])])])]);
                        Node
                        ("d",
                        [Node
                            ("0007",
                            [Node ("Y", []);
                                Node
                                ("4",
                                [Node ("0024", []); Node ("z", []); Node ("0127", []);
                                    Node (" ", []); Node ("0027", [])])])]);
                        Node ("0019", []); Node ("n", []); Node ("0029", [])]);
                    Node ("V", []); Node ("1", []); Node ("`", []);
                    Node (";", [Node ("u", [])]); Node ("j", [])])]); Node ("A", []);
            Node ("p", []); Node ("K", []);
            Node
            ("z",
            [Node ("0013", []); Node ("<", []); Node ("0023", []);
                Node ("F", [Node ("0030", [Node ("a", [])])]); Node ("!", []);
                Node ("P", []); Node ("+", []); Node ("Z", []); Node ("5", []);
                Node ("d", []); Node ("?", []);
                Node
                ("n",
                [Node ("0023", []); Node ("F", []); Node ("!", []); Node ("P", []);
                    Node ("+", []);
                Node
                    ("4",
                    [Node ("Q", []); Node ("0000", []); Node ("[", []);
                        Node ("0010", []);
                        Node
                        ("e", [Node ("0020", []); Node ("o", []); Node ("0030", [])])]);
                    Node ("0031", []); Node ("h", []); Node ("1", []); Node ("0012", []);
                    Node (";", []); Node ("0022", []);
                    Node
                    ("E",
                    [Node
                        ("f",
                        [Node
                            ("I",
                            [Node ("c", []); Node (">", [Node ("lala", [])]);
                                Node ("m", [Node ("Q", [])])]);
                            Node ("$", [Node ("#", [Node ("0000", [])])])]);
                        Node ("0021", [])]); Node (" ", []); Node ("O", []);
                    Node ("*", []); Node ("Y", []); Node ("4", []); Node ("c", []);
                    Node ("a", []);
                    Node
                    ("<",
                    [Node
                        ("_",
                        [Node ("7", []); Node ("0018", []);
                            Node ("A", [Node ("0026", [Node ("$", [])])]);
                            Node
                            ("0028",
                            [Node ("0004", []);
                                Node
                                ("_", [Node ("9", []); Node ("[", []); Node (" ", [])]);
                                Node ("0010", []); Node ("0127", [])]);
                            Node ("K", [Node ("=", [])])]); Node ("0014", [])])])]);
            Node ("U", [Node ("w", [])]); Node ("0004", []); Node ("_", []);
            Node ("0014", []); Node ("i", []); Node ("0024", []);
            Node ("s", [Node ("p", [])]); Node ("asda", [tt])])

    //let t = Node('a', [Node('b', [tt]); Node('c', [ttt])])
    let s = "(string label |> List.ofSeq |> List.head |> string)"
    let t2 = Node(String.replicate 50 s, [tttt; tttt])
    let t3 = Node("aldjasidoj", [t2; tttt])
    let t4 = Node("lalala", [tlong])
    let t5 = Node("lajda", [t4])
    let t6 = Node("ladjs", [t5])
    let t7 = Node("root", [t2; t5])
    tlong |> generateChart 1.0 |> showChart

    0
