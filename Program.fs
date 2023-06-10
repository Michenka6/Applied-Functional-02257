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

    let t = Node('\023',
        [Node ('k', [Node ('e', []); Node ('\031', [])]);
            Node
            ('-',
            [Node ('B', []); Node ('q', []); Node ('L', []); Node ('\020', []);
                Node ('q', []); Node ('n', []); Node ('2', []); Node ('$', []);
                Node ('a', []); Node ('S', []); Node ('X', []); Node ('n', []);
                Node ('"', []); Node (',', []); Node ('L', []); Node ('4', []);
                Node ('N', []); Node ('v', []); Node ('+', []); Node ('a', []);
                Node (' ', []); Node ('A', []); Node ('\\', []);
                Node ('H', [Node ('t', [])]); Node ('=', []);
                Node
                ('l',
                [Node ('F', []); Node ('u', [Node ('\021', []); Node ('p', [])]);
                    Node ('P', [])]); Node ('G', []); Node ('v', []); Node ('Q', []);
                Node ('\000', []); Node ('[', []); Node ('\010', []); Node ('e', []);
                Node ('\020', []); Node ('o', []); Node ('\030', []); Node ('y', []);
                Node ('(', []); Node ('\003', []);
                Node
                ('2',
                [Node ('8', []);
                    Node
                    ('g',
                    [Node ('\015', [Node ('u', [Node ('p', [])]); Node ('P', [])])]);
                    Node
                    ('B',
                    [Node
                        ('O',
                        [Node ('g', [Node ('W', [])]);
                            Node ('\022', [Node ('\016', [])])])]);
                    Node
                    ('q',
                    [Node ('9', []);
                        Node
                        ('\020', [Node ('\011', [Node ('\022', [Node ('\029', [])])])])]);
                    Node
                    ('L',
                    [Node ('O', []);
                        Node
                        ('~',
                        [Node
                            ('-', [Node ('r', [Node ('G', []); Node ('\019', [])])]);
                            Node ('M', [])])]); Node ('{', []); Node ('V', []);
                    Node ('\005', [Node ('\022', [])]); Node ('`', []);
                    Node
                    ('\015',
                    [Node
                        ('s',
                        [Node
                            ('\018', [Node ('{', []); Node ('2', [Node ('c', [])])])])]);
                    Node ('E', []); Node ('\016', []); Node ('\018', [Node ('O', [])]);
                    Node ('~', [Node ('w', [Node ('E', [])])]);
                    Node ('u', [Node ('\023', [Node ('K', []); Node ('&', [])])]);
                    Node
                    ('&',
                    [Node ('(', [Node ('\013', [Node ('0', [Node ('a', [])])])]);
                        Node ('-', [])]); Node ('$', []);
                    Node ('\009', [Node ('Y', []); Node ('\020', [])]);
                    Node ('_', [Node ('\000', [])]);
                    Node
                    ('\021',
                    [Node ('G', []); Node ('\003', [Node ('\029', [Node ('T', [])])])]);
                    Node ('\026', []); Node ('/', []);
                    Node ('<', [Node ('5', []); Node ('\007', [])]);
                    Node (')', [Node ('s', [])]);
                    Node
                    ('z',
                    [Node ('p', [Node ('\002', [])]);
                        Node
                        ('s',
                        [Node
                            ('r',
                            [Node ('\012', []); Node ('\022', []);
                                Node ('_', [Node ('<', []); Node ('\b', [])])]);
                            Node ('\009', []); Node ('~', []);
                            Node ('-', [Node ('\005', [])])]); Node ('C', []);
                        Node ('\026', [])]);
                    Node
                    ('V',
                    [Node ('_', []);
                        Node ('T', [Node ('+', [Node ('\000', [Node ('p', [])])])])])]);
                Node ('\013', []); Node ('<', []); Node ('\023', []); Node ('F', []);
                Node ('!', []); Node ('P', []); Node ('+', []); Node ('Z', []);
                Node ('5', []); Node ('d', []); Node ('?', []); Node ('n', []);
                Node ('q', []); Node ('f', []); Node ('\021', []); Node ('p', []);
                Node ('\031', []); Node ('z', []); Node (')', []); Node ('\004', [])]);
            Node (',', [Node ('}', [])]); Node ('<', []);
            Node
            ('&',
            [Node ('g', [Node ('Q', [Node ('\010', [])]); Node (',', [])]);
                Node
                ('B',
                [Node ('g', []); Node ('\022', []); Node ('#', []); Node ('i', []);
                    Node ('\000', []); Node ('\014', []); Node ('3', []); Node ('(', []);
                    Node ('W', []); Node ('2', []);
                    Node
                    ('a',
                    [Node
                        ('\002',
                        [Node ('q', []);
                            Node ('L', [Node ('r', []); Node ('M', [Node ('1', [])])]);
                            Node ('{', [Node ('\b', [])]); Node ('V', []);
                            Node
                            ('\005',
                            [Node ('\b', [Node ('\015', []); Node ('1', [])])])])]);
                    Node ('<', []);
                    Node
                    ('k',
                    [Node
                        ('{',
                        [Node ('0', []); Node ('\011', []); Node (':', []);
                            Node ('\021', []);
                            Node
                            ('D',
                            [Node ('L', [Node (')', [])]); Node ('\'', []);
                                Node ('V', [Node ('3', [])]); Node ('1', []);
                                Node ('/', [Node ('v', [])]); Node ('^', [])]);
                            Node ('\031', [])]);
                        Node
                        ('*',
                        [Node ('F', [Node ('h', [])]); Node ('K', []);
                            Node ('z', [Node ('h', [])]); Node ('U', [])]);
                        Node
                        ('\005',
                        [Node ('\006', []); Node ('\004', []);
                            Node ('3', [Node ('Z', [])]); Node ('\014', []);
                            Node ('=', [])])]); Node ('F', []); Node ('u', [])]);
                Node ('q', []); Node ('L', []); Node ('J', [])])])
    

    let t2 = Node ('\000',
        [Node ('(', []); Node ('\003', []); Node ('2', []); Node ('\013', []);
            Node
            ('<',
            [Node ('0', []); Node ('_', []); Node (':', []);
                Node
                ('i',
                [Node ('[', [Node ('i', [Node ('F', [])])]); Node ('\010', []);
                    Node ('e', [Node ('i', [Node ('\005', [])])]); Node ('\020', [])]);
                Node ('D', [])]); Node ('\023', []);
            Node
            ('F',
            [Node ('0', []);
                Node
                ('_',
                [Node ('\026', []); Node ('I', [Node ('Z', [])]);
                    Node
                    ('$',
                    [Node
                        ('D',
                        [Node
                            ('\002', [Node ('^', [Node ('B', []); Node ('\007', [])])])])])])]);
            Node ('!', []); Node ('P', []); Node ('+', []); Node ('Z', []);
            Node ('5', []); Node ('d', []); Node ('?', []); Node ('n', []);
            Node ('I', [Node ('\019', [Node ('k', [Node ('O', [])])])]); Node ('x', []);
            Node ('}', []); Node ('X', [Node ('"', [])]); Node ('\007', []);
            Node
            ('b',
            [Node
                ('"',
                [Node ('(', []);
                    Node ('\003', [Node ('\009', [Node ('N', [])]); Node ('d', [])]);
                    Node ('2', []);
                    Node
                    ('\013', [Node ('\009', []); Node ('d', []); Node ('\019', [])]);
                    Node ('<', [])]); Node ('Q', []); Node (',', []);
                Node ('[', [Node ('>', [])]); Node ('6', [Node ('(', [])]);
                Node ('e', []); Node ('@', []); Node ('o', []); Node ('J', []);
                Node ('y', []); Node ('T', []); Node ('\003', []); Node ('^', [])]);
            Node ('\017', []); Node ('l', []); Node ('\027', []); Node ('v', []);
            Node ('%', []); Node ('\000', []); Node ('~', []);
            Node
            ('-',
            [Node
                ('\027',
                [Node ('\029', []); Node ('x', []); Node ('\'', []);
                    Node ('\002', []);
                    Node
                    ('1',
                    [Node ('*', []); Node ('\005', []);
                        Node
                        ('4',
                        [Node ('O', []);
                            Node
                            ('~',
                            [Node ('\027', []); Node ('J', []);
                                Node
                                ('%',
                                [Node ('\002', []); Node ('N', []); Node ('\019', []);
                                    Node ('\009', []); Node ('+', []); Node ('!', []);
                                    Node ('<', []); Node ('2', [])]); Node ('T', []);
                                Node ('/', [])]); Node ('Y', []); Node ('\b', []);
                            Node ('\006', [])]); Node ('\015', []); Node ('>', []);
                        Node ('\025', []); Node ('\023', []); Node ('F', []);
                        Node ('!', [Node ('r', []); Node ('M', []); Node ('|', [])])])]);
                Node ('J', []); Node ('%', [Node ('s', [])]); Node ('T', []);
                Node ('/', []); Node ('^', []); Node ('9', [Node ('\022', [])]);
                Node ('h', []); Node ('C', []); Node ('r', []); Node ('M', []);
                Node ('|', []); Node ('W', []); Node ('\006', []);
                Node ('a', [Node ('\015', [Node ('$', [])])]);
                Node
                ('\016',
                [Node
                    ('%',
                    [Node ('G', []);
                        Node
                        ('v',
                        [Node ('0', []);
                            Node ('\011', [Node ('$', [Node ('\001', [])])])])]);
                    Node ('T', [])]); Node ('k', []); Node ('\026', [])]);
            Node ('\b', []);
            Node
            ('7',
            [Node
                ('\027',
                [Node ('U', [Node ('\026', [])]); Node ('0', []);
                    Node ('_', [Node ('D', [Node ('\'', [])])]); Node (':', []);
                    Node ('i', [Node ('\019', [Node ('_', [Node ('g', [])])])])])]);
            Node ('\018', []); Node ('A', []); Node ('\028', []); Node ('K', []);
            Node ('&', []); Node ('U', []); Node ('0', []); Node ('_', []);
            Node (':', []); Node ('i', []); Node ('D', []); Node ('s', []);
            Node ('N', []); Node ('}', []); Node ('X', [Node ('w', []); Node ('R', [])]);
            Node ('\007', [])])

    let t3 = Node ('\000',
        [Node ('(', []); Node ('\003', []); Node ('2', []); Node ('\013', []);
            Node
            ('<',
            [Node ('0', []); Node ('_', []); Node (':', []);
                Node
                ('i',
                [Node ('[', [Node ('i', [Node ('F', [])])]); Node ('\010', []);
                    Node ('e', [Node ('i', [Node ('\005', [])])]); Node ('\020', [])]);
                Node ('D', [])]); Node ('\023', []);
            Node
            ('F',
            [Node ('0', []);
                Node
                ('_',
                [Node ('\026', []); Node ('I', [Node ('Z', [])]);
                    Node
                    ('$',
                    [Node
                        ('D',
                        [Node
                            ('\002', [Node ('^', [Node ('B', []); Node ('\007', [])])])])])])]);
            Node ('!', []); Node ('P', []); Node ('+', []); Node ('Z', []);
            Node ('5', []); Node ('d', []); Node ('?', []); Node ('n', []);
            Node ('I', [Node ('\019', [Node ('k', [Node ('O', [])])])]); Node ('x', []);
            Node ('}', []); Node ('X', [Node ('"', [])]); Node ('\007', []);
            Node
            ('b',
            [Node
                ('"',
                [Node ('(', []);
                    Node ('\003', [Node ('\009', [Node ('N', [])]); Node ('d', [])]);
                    Node ('2', []);
                    Node
                    ('\013', [Node ('\009', []); Node ('d', []); Node ('\019', [])]);
                    Node ('<', [])]); Node ('Q', []); Node (',', []);
                Node ('[', [Node ('>', [])]); Node ('6', [Node ('(', [])]);
                Node ('e', []); Node ('@', []); Node ('o', []); Node ('J', []);
                Node ('y', []); Node ('T', []); Node ('\003', []); Node ('^', [])]);
            Node ('\017', []); Node ('l', []); Node ('\027', []); Node ('v', []);
            Node ('%', []); Node ('\000', []); Node ('~', []);
            Node
            ('-',
            [Node
                ('\027',
                [Node ('\029', []); Node ('x', []); Node ('\'', []);
                    Node ('\002', []);
                    Node
                    ('1',
                    [Node ('*', []); Node ('\005', []);
                        Node
                        ('4',
                        [Node ('O', []);
                            Node
                            ('~',
                            [Node ('\027', []); Node ('J', []);
                                Node
                                ('%',
                                [Node ('\002', []); Node ('N', []); Node ('\019', []);
                                    Node ('\009', []); Node ('+', []); Node ('!', []);
                                    Node ('<', []); Node ('2', [])]); Node ('T', []);
                                Node ('/', [])]); Node ('Y', []); Node ('\b', []);
                            Node ('\006', [])]); Node ('\015', []); Node ('>', []);
                        Node ('\025', []); Node ('\023', []); Node ('F', []);
                        Node ('!', [Node ('r', []); Node ('M', []); Node ('|', [])])])]);
                Node ('J', []); Node ('%', [Node ('s', [])]); Node ('T', []);
                Node ('/', []); Node ('^', []); Node ('9', [Node ('\022', [])]);
                Node ('h', []); Node ('C', []); Node ('r', []); Node ('M', []);
                Node ('|', []); Node ('W', []); Node ('\006', []);
                Node ('a', [Node ('\015', [Node ('$', [])])]);
                Node
                ('\016',
                [Node
                    ('%',
                    [Node ('G', []);
                        Node
                        ('v',
                        [Node ('0', []);
                            Node ('\011', [Node ('$', [Node ('\001', [t2])])])])]);
                    Node ('T', [])]); Node ('k', []); Node ('\026', [])]);
            Node ('\b', []);
            Node
            ('7',
            [Node
                ('\027',
                [Node ('U', [Node ('\026', [])]); Node ('0', []);
                    Node ('_', [Node ('D', [Node ('\'', [])])]); Node (':', []);
                    Node ('i', [Node ('\019', [Node ('_', [Node ('g', [])])])])])]);
            Node ('\018', []); Node ('A', []); Node ('\028', []); Node ('K', []);
            Node ('&', []); Node ('U', []); Node ('0', []); Node ('_', []);
            Node (':', []); Node ('i', []); Node ('D', []); Node ('s', []);
            Node ('N', []); Node ('}', []); Node ('X', [Node ('w', []); Node ('R', [])]);
            Node ('\007', [])])

    let t4 = Node('\023',
        [Node ('k', [Node ('e', []); Node ('\031', [])]);
            Node
            ('-',
            [Node ('B', []); Node ('q', []); Node ('L', []); Node ('\020', []);
                Node ('q', []); Node ('n', []); Node ('2', []); Node ('$', []);
                Node ('a', []); Node ('S', []); Node ('X', []); Node ('n', []);
                Node ('"', []); Node (',', []); Node ('L', []); Node ('4', []);
                Node ('N', []); Node ('v', []); Node ('+', []); Node ('a', []);
                Node (' ', []); Node ('A', []); Node ('\\', []);
                Node ('H', [Node ('t', [])]); Node ('=', []);
                Node
                ('l',
                [Node ('F', []); Node ('u', [Node ('\021', []); Node ('p', [])]);
                    Node ('P', [])]); Node ('G', []); Node ('v', []); Node ('Q', []);
                Node ('\000', []); Node ('[', []); Node ('\010', []); Node ('e', []);
                Node ('\020', []); Node ('o', []); Node ('\030', []); Node ('y', []);
                Node ('(', []); Node ('\003', []);
                Node
                ('2',
                [Node ('8', []);
                    Node
                    ('g',
                    [Node ('\015', [Node ('u', [Node ('p', [])]); Node ('P', [])])]);
                    Node
                    ('B',
                    [Node
                        ('O',
                        [Node ('g', [Node ('W', [])]);
                            Node ('\022', [Node ('\016', [])])])]);
                    Node
                    ('q',
                    [Node ('9', []);
                        Node
                        ('\020', [Node ('\011', [Node ('\022', [Node ('\029', [])])])])]);
                    Node
                    ('L',
                    [Node ('O', []);
                        Node
                        ('~',
                        [Node
                            ('-', [Node ('r', [Node ('G', []); Node ('\019', [])])]);
                            Node ('M', [])])]); Node ('{', []); Node ('V', []);
                    Node ('\005', [Node ('\022', [])]); Node ('`', []);
                    Node
                    ('\015',
                    [Node
                        ('s',
                        [Node
                            ('\018', [Node ('{', []); Node ('2', [Node ('c', [])])])])]);
                    Node ('E', []); Node ('\016', []); Node ('\018', [Node ('O', [])]);
                    Node ('~', [Node ('w', [Node ('E', [])])]);
                    Node ('u', [Node ('\023', [Node ('K', []); Node ('&', [])])]);
                    Node
                    ('&',
                    [Node ('(', [Node ('\013', [Node ('0', [Node ('a', [])])])]);
                        Node ('-', [])]); Node ('$', []);
                    Node ('\009', [Node ('Y', []); Node ('\020', [])]);
                    Node ('_', [Node ('\000', [])]);
                    Node
                    ('\021',
                    [Node ('G', []); Node ('\003', [Node ('\029', [Node ('T', [])])])]);
                    Node ('\026', []); Node ('/', []);
                    Node ('<', [Node ('5', []); Node ('\007', [])]);
                    Node (')', [Node ('s', [])]);
                    Node
                    ('z',
                    [Node ('p', [Node ('\002', [])]);
                        Node
                        ('s',
                        [Node
                            ('r',
                            [Node ('\012', []); Node ('\022', []);
                                Node ('_', [Node ('<', []); Node ('\b', [t3])])]);
                            Node ('\009', []); Node ('~', []);
                            Node ('-', [Node ('\005', [])])]); Node ('C', []);
                        Node ('\026', [])]);
                    Node
                    ('V',
                    [Node ('_', []);
                        Node ('T', [Node ('+', [Node ('\000', [Node ('p', [])])])])])]);
                Node ('\013', []); Node ('<', []); Node ('\023', []); Node ('F', []);
                Node ('!', []); Node ('P', []); Node ('+', []); Node ('Z', []);
                Node ('5', []); Node ('d', []); Node ('?', []); Node ('n', []);
                Node ('q', []); Node ('f', []); Node ('\021', []); Node ('p', []);
                Node ('\031', []); Node ('z', []); Node (')', []); Node ('\004', [])]);
            Node (',', [Node ('}', [])]); Node ('<', []);
            Node
            ('&',
            [Node ('g', [Node ('Q', [Node ('\010', [])]); Node (',', [])]);
                Node
                ('B',
                [Node ('g', []); Node ('\022', []); Node ('#', []); Node ('i', []);
                    Node ('\000', []); Node ('\014', []); Node ('3', []); Node ('(', []);
                    Node ('W', []); Node ('2', []);
                    Node
                    ('a',
                    [Node
                        ('\002',
                        [Node ('q', []);
                            Node ('L', [Node ('r', []); Node ('M', [Node ('1', [])])]);
                            Node ('{', [Node ('\b', [])]); Node ('V', []);
                            Node
                            ('\005',
                            [Node ('\b', [Node ('\015', []); Node ('1', [])])])])]);
                    Node ('<', []);
                    Node
                    ('k',
                    [Node
                        ('{',
                        [Node ('0', []); Node ('\011', []); Node (':', []);
                            Node ('\021', []);
                            Node
                            ('D',
                            [Node ('L', [Node (')', [])]); Node ('\'', []);
                                Node ('V', [Node ('3', [])]); Node ('1', []);
                                Node ('/', [Node ('v', [])]); Node ('^', [])]);
                            Node ('\031', [])]);
                        Node
                        ('*',
                        [Node ('F', [Node ('h', [])]); Node ('K', []);
                            Node ('z', [Node ('h', [])]); Node ('U', [])]);
                        Node
                        ('\005',
                        [Node ('\006', []); Node ('\004', []);
                            Node ('3', [Node ('Z', [])]); Node ('\014', []);
                            Node ('=', [])])]); Node ('F', []); Node ('u', [])]);
                Node ('q', []); Node ('L', []); Node ('J', [])])])


    t |> generateChart 1.0 |> showChart

    0
