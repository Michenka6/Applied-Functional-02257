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

    let t = Node("A",
        [Node ("meaningful connections", [Node ("meaningful connections", [])]); Node ("\030", []); Node ("meaningful connections", []);
            Node ("meaningful connections", []); Node ("meaningful connections", [Node ("meaningful connections", [])]); Node ("meaningful connections", []); Node ("meaningful connections", []);
            Node ("\127", []); Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("\001", []);
            Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", [Node ("meaningful connections", [])]);
            Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("\007", []);
            Node ("meaningful connections", []); Node ("\017", []);
            Node
            ("meaningful connections",
            [Node
                ("meaningful connections",
                [Node ("meaningful connections", [Node ("\027", []); Node ("meaningful connections", [])]); Node ("\005", [])])]);
            Node ("\027", []); Node ("meaningful connections", []); Node ("meaningful connections", []);
            Node ("dasdas", [Node ("\127", [Node ("meaningful connections", [Node ("\030", [])])])]);
            Node ("meaningful connections", []);
            Node
            ("\010",
            [Node ("\127", [Node ("meaningful connections", [Node ("meaningful connections", [Node ("meaningful connections", [])])])]);
                Node ("meaningful connections", []); Node ("\009", []); Node ("\014", []);
                Node ("meaningful connections", [Node ("\005", [Node ("meaningful connections", [Node ("meaningful connections", [])])])]);
                Node ("\024", []); Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []);
                Node ("meaningful connections", []); Node ("meaningful connections", [Node ("meaningful connections", [])]); Node ("meaningful connections", []);
                Node ("meaningful connections", [Node ("meaningful connections", [])]); Node ("meaningful connections", []); Node ("meaningful connections", []);
                Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("\003", []);
                Node ("meaningful connections", [Node ("meaningful connections", [Node ("meaningful connections", [Node ("\\", [Node ("meaningful connections", [])])])])]);
                Node ("\013", []); Node ("meaningful connections", [Node ("meaningful connections", [])]); Node ("\023", []);
                Node ("meaningful connections", [Node ("meaningful connections", [])]); Node ("meaningful connections", []); Node ("meaningful connections", []);
                Node ("meaningful connections", []); Node ("\017", []); Node ("meaningful connections", []); Node ("meaningful connections", []);
                Node ("meaningful connections", [Node ("\021", [])]); Node ("meaningful connections", []); Node ("meaningful connections", []);
                Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []);
                Node ("\000", []); Node ("meaningful connections", []); Node ("\010", []); Node ("meaningful connections", []);
                Node ("\020", []); Node ("meaningful connections", []); Node ("meaningful connections", []);
                Node ("meaningful connections", [Node ("\012", [])]); Node ("meaningful connections", []); Node ("meaningful connections", []);
                Node ("meaningful connections", []); Node ("\127", []); Node ("meaningful connections", []);
                Node ("\009", [Node ("meaningful connections", [])]); Node ("meaningful connections", []); Node ("\019", []);
                Node ("meaningful connections", []); Node ("\029", []); Node ("meaningful connections", []); Node ("meaningful connections", []);
                Node ("meaningful connections", []);
                Node
                ("\007",
                [Node ("meaningful connections", [Node ("\030", [])]); Node ("meaningful connections", []);
                    Node ("meaningful connections", [Node ("\030", [])]); Node ("meaningful connections", []);
                    Node ("\127", [Node ("meaningful connections", [])]); Node ("meaningful connections", []);
                    Node ("\009", [Node ("meaningful connections", [Node ("meaningful connections", [Node ("meaningful connections", [])])])]);
                    Node
                    ("meaningful connections",
                    [Node
                        ("meaningful connections",
                        [Node ("meaningful connections", [Node ("meaningful connections", [Node ("\127", [])])]);
                            Node ("meaningful connections", []);
                            Node ("\000", [Node ("\027", [Node ("\127", [])])]);
                            Node ("meaningful connections", []);
                            Node
                            ("\010",
                            [Node ("\027", [Node ("meaningful connections", [])]);
                                Node ("meaningful connections", [Node ("meaningful connections", [])]);
                                Node ("meaningful connections", [Node ("meaningful connections", []); Node ("meaningful connections", [])])]);
                            Node ("meaningful connections", []);
                            Node ("\020", [Node ("meaningful connections", [Node ("meaningful connections", [])])]);
                            Node ("meaningful connections", []); Node ("\030", [Node ("\020", [])]);
                            Node ("meaningful connections", []); Node ("meaningful connections", [Node ("\020", [])]);
                            Node ("meaningful connections", []); Node ("meaningful connections", [Node ("meaningful connections", [Node ("meaningful connections", [])])]);
                            Node ("meaningful connections", []); Node ("meaningful connections", [Node ("meaningful connections", [])]); Node ("meaningful connections", []);
                            Node ("meaningful connections", [Node ("\020", [Node ("meaningful connections", [])])])])]);
                    Node ("\019", []); Node ("meaningful connections", []);
                    Node ("\029", [Node ("\023", [Node ("meaningful connections", [])])]); Node ("meaningful connections", []);
                    Node ("meaningful connections", []); Node ("\002", [])]); Node ("meaningful connections", []);
                Node ("\017", []); Node ("meaningful connections", []);
                Node
                ("\027",
                [Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []);
                    Node ("meaningful connections", []);
                    Node
                    ("meaningful connections",
                    [Node
                        ("meaningful connections",
                        [Node
                            ("\014",
                            [Node
                                ("\005",
                                [Node ("meaningful connections", []); Node ("\004", []); Node ("meaningful connections", []);
                                Node ("\021", [])])]); Node ("meaningful connections", [])])]);
                    Node
                    ("meaningful connections",
                    [Node ("meaningful connections", [Node ("meaningful connections", [])]); Node ("\026", []);
                        Node ("meaningful connections", [Node ("\000", [])]); Node ("meaningful connections", []);
                        Node ("\127", []); Node ("meaningful connections", [])]); Node ("\007", []);
                    Node ("meaningful connections", []); Node ("\017", []); Node ("meaningful connections", []); Node ("\027", []);
                    Node ("meaningful connections", [])])]); Node ("meaningful connections", []);
        Node
        ("\020",
        [Node
            ("meaningful connections",
            [Node ("meaningful connections", []);
                Node
                ("meaningful connections",
                [Node ("meaningful connections", [Node ("\013", []); Node ("meaningful connections", [])]);
                    Node ("\016", [])]); Node ("meaningful connections", [])])]); Node ("meaningful connections", []);
        Node ("\030", [Node ("meaningful connections", [])]); Node ("\028", []); Node ("meaningful connections", []);
        Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []);
        Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []);
        Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("\007", []);
        Node ("meaningful connections", []); Node ("\017", []); Node ("meaningful connections", []); Node ("\027", []);
        Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("\000", []); Node ("meaningful connections", []);
        Node ("\010", []); Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", [])])


    let t1 = Node("A",
        [Node ("meaningful connections", [Node ("meaningful connections", []); Node ("\031", [])]);
            Node
            ("meaningful connections",
            [Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("\020", []);
                Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []);
                Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []);
                Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []);
                Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []);
                Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("\\", []);
                Node ("meaningful connections", [Node ("meaningful connections", [])]); Node ("meaningful connections", []);
                Node
                ("meaningful connections",
                [Node ("meaningful connections", []); Node ("meaningful connections", [Node ("\021", []); Node ("meaningful connections", [])]);
                    Node ("meaningful connections", [])]); Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []);
                Node ("\000", []); Node ("meaningful connections", []); Node ("\010", []); Node ("meaningful connections", []);
                Node ("\020", []); Node ("meaningful connections", []); Node ("\030", []); Node ("meaningful connections", []);
                Node ("meaningful connections", []); Node ("\003", []);
                Node
                ("meaningful connections",
                [Node ("meaningful connections", []);
                    Node
                    ("meaningful connections",
                    [Node ("\015", [Node ("meaningful connections", [Node ("meaningful connections", [])]); Node ("meaningful connections", [])])]);
                    Node
                    ("meaningful connections",
                    [Node
                        ("meaningful connections",
                        [Node ("meaningful connections", [Node ("meaningful connections", [])]);
                            Node ("\022", [Node ("\016", [])])])]);
                    Node
                    ("meaningful connections",
                    [Node ("meaningful connections", []);
                        Node
                        ("\020", [Node ("\011", [Node ("\022", [Node ("\029", [])])])])]);
                    Node
                    ("meaningful connections",
                    [Node ("meaningful connections", []);
                        Node
                        ("meaningful connections",
                        [Node
                            ("meaningful connections", [Node ("meaningful connections", [Node ("meaningful connections", []); Node ("\019", [])])]);
                            Node ("meaningful connections", [])])]); Node ("meaningful connections", []); Node ("meaningful connections", []);
                    Node ("\005", [Node ("\022", [])]); Node ("meaningful connections", []);
                    Node
                    ("\015",
                    [Node
                        ("meaningful connections",
                        [Node
                            ("\018", [Node ("meaningful connections", []); Node ("meaningful connections", [Node ("meaningful connections", [])])])])]);
                    Node ("meaningful connections", []); Node ("\016", []); Node ("\018", [Node ("meaningful connections", [])]);
                    Node ("meaningful connections", [Node ("meaningful connections", [Node ("meaningful connections", [])])]);
                    Node ("meaningful connections", [Node ("\023", [Node ("meaningful connections", []); Node ("meaningful connections", [])])]);
                    Node
                    ("meaningful connections",
                    [Node ("meaningful connections", [Node ("\013", [Node ("meaningful connections", [Node ("meaningful connections", [])])])]);
                        Node ("meaningful connections", [])]); Node ("meaningful connections", []);
                    Node ("\009", [Node ("meaningful connections", []); Node ("\020", [])]);
                    Node ("meaningful connections", [Node ("\000", [])]);
                    Node
                    ("\021",
                    [Node ("meaningful connections", []); Node ("\003", [Node ("\029", [Node ("meaningful connections", [])])])]);
                    Node ("\026", []); Node ("meaningful connections", []);
                    Node ("meaningful connections", [Node ("meaningful connections", []); Node ("\007", [])]);
                    Node ("meaningful connections", [Node ("meaningful connections", [])]);
                    Node
                    ("meaningful connections",
                    [Node ("meaningful connections", [Node ("\002", [])]);
                        Node
                        ("meaningful connections",
                        [Node
                            ("meaningful connections",
                            [Node ("\012", []); Node ("\022", []);
                                Node ("meaningful connections", [Node ("meaningful connections", []); Node ("\b", [])])]);
                            Node ("\009", []); Node ("meaningful connections", []);
                            Node ("meaningful connections", [Node ("\005", [])])]); Node ("meaningful connections", []);
                        Node ("\026", [])]);
                    Node
                    ("meaningful connections",
                    [Node ("meaningful connections", []);
                        Node ("meaningful connections", [Node ("meaningful connections", [Node ("\000", [Node ("meaningful connections", [])])])])])]);
                Node ("\013", []); Node ("meaningful connections", []); Node ("\023", []); Node ("meaningful connections", []);
                Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []);
                Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []);
                Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("\021", []); Node ("meaningful connections", []);
                Node ("\031", []); Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("\004", [])]);
            Node ("meaningful connections", [Node ("meaningful connections", [])]); Node ("meaningful connections", []);
            Node
            ("meaningful connections",
            [Node ("meaningful connections", [Node ("meaningful connections", [Node ("\010", [])]); Node ("meaningful connections", [])]);
                Node
                ("meaningful connections",
                [Node ("meaningful connections", []); Node ("\022", []); Node ("meaningful connections", []); Node ("meaningful connections", []);
                    Node ("\000", []); Node ("\014", []); Node ("meaningful connections", []); Node ("meaningful connections", []);
                    Node ("meaningful connections", []); Node ("meaningful connections", []);
                    Node
                    ("meaningful connections",
                    [Node
                        ("\002",
                        [Node ("meaningful connections", []);
                            Node ("meaningful connections", [Node ("meaningful connections", []); Node ("meaningful connections", [Node ("meaningful connections", [])])]);
                            Node ("meaningful connections", [Node ("\b", [])]); Node ("meaningful connections", []);
                            Node
                            ("\005",
                            [Node ("\b", [Node ("\015", []); Node ("meaningful connections", [])])])])]);
                    Node ("meaningful connections", []);
                    Node
                    ("meaningful connections",
                    [Node
                        ("meaningful connections",
                        [Node ("meaningful connections", []); Node ("\011", []); Node ("meaningful connections", []);
                            Node ("\021", []);
                            Node
                            ("meaningful connections",
                            [Node ("meaningful connections", [Node ("meaningful connections", [])]); Node ("meaningful connections", []);
                                Node ("meaningful connections", [Node ("meaningful connections", [])]); Node ("meaningful connections", []);
                                Node ("meaningful connections", [Node ("meaningful connections", [])]); Node ("meaningful connections", [])]);
                            Node ("\031", [])]);
                        Node
                        ("meaningful connections",
                        [Node ("meaningful connections", [Node ("meaningful connections", [])]); Node ("meaningful connections", []);
                            Node ("meaningful connections", [Node ("meaningful connections", [])]); Node ("meaningful connections", [])]);
                        Node
                        ("\005",
                        [Node ("\006", []); Node ("\004", []);
                            Node ("meaningful connections", [Node ("meaningful connections", [])]); Node ("\014", []);
                            Node ("meaningful connections", [])])]); Node ("meaningful connections", []); Node ("meaningful connections", [])]);
                Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", [])])])
    

    let t2 = Node ("B",
        [Node ("meaningful connections", []); Node ("\003", []); Node ("meaningful connections", []); Node ("\013", []);
            Node
            ("meaningful connections",
            [Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []);
                Node
                ("meaningful connections",
                [Node ("meaningful connections", [Node ("meaningful connections", [Node ("meaningful connections", [])])]); Node ("\010", []);
                    Node ("meaningful connections", [Node ("meaningful connections", [Node ("\005", [])])]); Node ("\020", [])]);
                Node ("meaningful connections", [])]); Node ("\023", []);
            Node
            ("meaningful connections",
            [Node ("meaningful connections", []);
                Node
                ("meaningful connections",
                [Node ("\026", []); Node ("meaningful connections", [Node ("meaningful connections", [])]);
                    Node
                    ("meaningful connections",
                    [Node
                        ("meaningful connections",
                        [Node
                            ("\002", [Node ("meaningful connections", [Node ("meaningful connections", []); Node ("\007", [])])])])])])]);
            Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []);
            Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []);
            Node ("meaningful connections", [Node ("\019", [Node ("meaningful connections", [Node ("meaningful connections", [])])])]); Node ("meaningful connections", []);
            Node ("meaningful connections", []); Node ("meaningful connections", [Node ("meaningful connections", [])]); Node ("\007", []);
            Node
            ("meaningful connections",
            [Node
                ("meaningful connections",
                [Node ("meaningful connections", []);
                    Node ("\003", [Node ("\009", [Node ("meaningful connections", [])]); Node ("meaningful connections", [])]);
                    Node ("meaningful connections", []);
                    Node
                    ("\013", [Node ("\009", []); Node ("meaningful connections", []); Node ("\019", [])]);
                    Node ("meaningful connections", [])]); Node ("meaningful connections", []); Node ("meaningful connections", []);
                Node ("meaningful connections", [Node ("meaningful connections", [])]); Node ("meaningful connections", [Node ("meaningful connections", [])]);
                Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []);
                Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("\003", []); Node ("meaningful connections", [])]);
            Node ("\017", []); Node ("meaningful connections", []); Node ("\027", []); Node ("meaningful connections", []);
            Node ("meaningful connections", []); Node ("\000", []); Node ("meaningful connections", []);
            Node
            ("meaningful connections",
            [Node
                ("\027",
                [Node ("\029", []); Node ("meaningful connections", []); Node ("meaningful connections", []);
                    Node ("\002", []);
                    Node
                    ("meaningful connections",
                    [Node ("meaningful connections", []); Node ("\005", []);
                        Node
                        ("meaningful connections",
                        [Node ("meaningful connections", []);
                            Node
                            ("meaningful connections",
                            [Node ("\027", []); Node ("meaningful connections", []);
                                Node
                                ("meaningful connections",
                                [Node ("\002", []); Node ("meaningful connections", []); Node ("\019", []);
                                    Node ("\009", []); Node ("meaningful connections", []); Node ("meaningful connections", []);
                                    Node ("meaningful connections", []); Node ("meaningful connections", [])]); Node ("meaningful connections", []);
                                Node ("meaningful connections", [])]); Node ("meaningful connections", []); Node ("\b", []);
                            Node ("\006", [])]); Node ("\015", []); Node ("meaningful connections", []);
                        Node ("\025", []); Node ("\023", []); Node ("meaningful connections", []);
                        Node ("meaningful connections", [Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", [])])])]);
                Node ("meaningful connections", []); Node ("meaningful connections", [Node ("meaningful connections", [])]); Node ("meaningful connections", []);
                Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", [Node ("\022", [])]);
                Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []);
                Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("\006", []);
                Node ("meaningful connections", [Node ("\015", [Node ("meaningful connections", [])])]);
                Node
                ("\016",
                [Node
                    ("meaningful connections",
                    [Node ("meaningful connections", []);
                        Node
                        ("meaningful connections",
                        [Node ("meaningful connections", []);
                            Node ("\011", [Node ("meaningful connections", [Node ("\001", [])])])])]);
                    Node ("meaningful connections", [])]); Node ("meaningful connections", []); Node ("\026", [])]);
            Node ("\b", []);
            Node
            ("meaningful connections",
            [Node
                ("\027",
                [Node ("meaningful connections", [Node ("\026", [])]); Node ("meaningful connections", []);
                    Node ("meaningful connections", [Node ("meaningful connections", [Node ("meaningful connections", [])])]); Node ("meaningful connections", []);
                    Node ("meaningful connections", [Node ("\019", [Node ("meaningful connections", [Node ("meaningful connections", [])])])])])]);
            Node ("\018", []); Node ("meaningful connections", []); Node ("\028", []); Node ("meaningful connections", []);
            Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []);
            Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []);
            Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", [Node ("meaningful connections", []); Node ("meaningful connections", [])]);
            Node ("\007", [])])

    let t3 = Node ("\000",
        [Node ("meaningful connections", []); Node ("\003", []); Node ("meaningful connections", []); Node ("\013", []);
            Node
            ("meaningful connections",
            [Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []);
                Node
                ("meaningful connections",
                [Node ("meaningful connections", [Node ("meaningful connections", [Node ("meaningful connections", [])])]); Node ("\010", []);
                    Node ("meaningful connections", [Node ("meaningful connections", [Node ("\005", [])])]); Node ("\020", [])]);
                Node ("meaningful connections", [])]); Node ("\023", []);
            Node
            ("meaningful connections",
            [Node ("meaningful connections", []);
                Node
                ("meaningful connections",
                [Node ("\026", []); Node ("meaningful connections", [Node ("meaningful connections", [])]);
                    Node
                    ("meaningful connections",
                    [Node
                        ("meaningful connections",
                        [Node
                            ("\002", [Node ("meaningful connections", [Node ("meaningful connections", []); Node ("\007", [])])])])])])]);
            Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []);
            Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []);
            Node ("meaningful connections", [Node ("\019", [Node ("meaningful connections", [Node ("meaningful connections", [])])])]); Node ("meaningful connections", []);
            Node ("meaningful connections", []); Node ("meaningful connections", [Node ("meaningful connections", [])]); Node ("\007", []);
            Node
            ("meaningful connections",
            [Node
                ("meaningful connections",
                [Node ("meaningful connections", []);
                    Node ("\003", [Node ("\009", [Node ("meaningful connections", [])]); Node ("meaningful connections", [])]);
                    Node ("meaningful connections", []);
                    Node
                    ("\013", [Node ("\009", []); Node ("meaningful connections", []); Node ("\019", [])]);
                    Node ("meaningful connections", [])]); Node ("meaningful connections", []); Node ("meaningful connections", []);
                Node ("meaningful connections", [Node ("meaningful connections", [])]); Node ("meaningful connections", [Node ("meaningful connections", [])]);
                Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []);
                Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("\003", []); Node ("meaningful connections", [])]);
            Node ("\017", []); Node ("meaningful connections", []); Node ("\027", []); Node ("meaningful connections", []);
            Node ("meaningful connections", []); Node ("\000", []); Node ("meaningful connections", []);
            Node
            ("meaningful connections",
            [Node
                ("\027",
                [Node ("\029", []); Node ("meaningful connections", []); Node ("meaningful connections", []);
                    Node ("\002", []);
                    Node
                    ("meaningful connections",
                    [Node ("meaningful connections", []); Node ("\005", []);
                        Node
                        ("meaningful connections",
                        [Node ("meaningful connections", []);
                            Node
                            ("meaningful connections",
                            [Node ("\027", []); Node ("meaningful connections", []);
                                Node
                                ("meaningful connections",
                                [Node ("\002", []); Node ("meaningful connections", []); Node ("\019", []);
                                    Node ("\009", []); Node ("meaningful connections", []); Node ("meaningful connections", []);
                                    Node ("meaningful connections", []); Node ("meaningful connections", [])]); Node ("meaningful connections", []);
                                Node ("meaningful connections", [])]); Node ("meaningful connections", []); Node ("\b", []);
                            Node ("\006", [])]); Node ("\015", []); Node ("meaningful connections", []);
                        Node ("\025", []); Node ("\023", []); Node ("meaningful connections", []);
                        Node ("meaningful connections", [Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", [])])])]);
                Node ("meaningful connections", []); Node ("meaningful connections", [Node ("meaningful connections", [])]); Node ("meaningful connections", []);
                Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", [Node ("\022", [])]);
                Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []);
                Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("\006", []);
                Node ("meaningful connections", [Node ("\015", [Node ("meaningful connections", [])])]);
                Node
                ("\016",
                [Node
                    ("meaningful connections",
                    [Node ("meaningful connections", []);
                        Node
                        ("meaningful connections",
                        [Node ("meaningful connections", []);
                            Node ("\011", [Node ("meaningful connections", [Node ("\001", [t2])])])])]);
                    Node ("meaningful connections", [])]); Node ("meaningful connections", []); Node ("\026", [])]);
            Node ("\b", []);
            Node
            ("meaningful connections",
            [Node
                ("\027",
                [Node ("meaningful connections", [Node ("\026", [])]); Node ("meaningful connections", []);
                    Node ("meaningful connections", [Node ("meaningful connections", [Node ("meaningful connections", [])])]); Node ("meaningful connections", []);
                    Node ("meaningful connections", [Node ("\019", [Node ("meaningful connections", [Node ("meaningful connections", [])])])])])]);
            Node ("\018", []); Node ("meaningful connections", []); Node ("\028", []); Node ("meaningful connections", []);
            Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []);
            Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []);
            Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", [Node ("meaningful connections", []); Node ("meaningful connections", [])]);
            Node ("\007", [])])

    let t4 = Node("\023",
        [Node ("meaningful connections", [Node ("meaningful connections", []); Node ("\031", [])]);
            Node
            ("meaningful connections",
            [Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("\020", []);
                Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []);
                Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []);
                Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []);
                Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []);
                Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("\\", []);
                Node ("meaningful connections", [Node ("meaningful connections", [])]); Node ("meaningful connections", []);
                Node
                ("meaningful connections",
                [Node ("meaningful connections", []); Node ("meaningful connections", [Node ("\021", []); Node ("meaningful connections", [])]);
                    Node ("meaningful connections", [])]); Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []);
                Node ("\000", []); Node ("meaningful connections", []); Node ("\010", []); Node ("meaningful connections", []);
                Node ("\020", []); Node ("meaningful connections", []); Node ("\030", []); Node ("meaningful connections", []);
                Node ("meaningful connections", []); Node ("\003", []);
                Node
                ("meaningful connections",
                [Node ("meaningful connections", []);
                    Node
                    ("meaningful connections",
                    [Node ("\015", [Node ("meaningful connections", [Node ("meaningful connections", [])]); Node ("meaningful connections", [])])]);
                    Node
                    ("meaningful connections",
                    [Node
                        ("meaningful connections",
                        [Node ("meaningful connections", [Node ("meaningful connections", [])]);
                            Node ("\022", [Node ("\016", [])])])]);
                    Node
                    ("meaningful connections",
                    [Node ("meaningful connections", []);
                        Node
                        ("\020", [Node ("\011", [Node ("\022", [Node ("\029", [])])])])]);
                    Node
                    ("meaningful connections",
                    [Node ("meaningful connections", []);
                        Node
                        ("meaningful connections",
                        [Node
                            ("meaningful connections", [Node ("meaningful connections", [Node ("meaningful connections", []); Node ("\019", [])])]);
                            Node ("meaningful connections", [])])]); Node ("meaningful connections", []); Node ("meaningful connections", []);
                    Node ("\005", [Node ("\022", [])]); Node ("meaningful connections", []);
                    Node
                    ("\015",
                    [Node
                        ("meaningful connections",
                        [Node
                            ("\018", [Node ("meaningful connections", []); Node ("meaningful connections", [Node ("meaningful connections", [])])])])]);
                    Node ("meaningful connections", []); Node ("\016", []); Node ("\018", [Node ("meaningful connections", [])]);
                    Node ("meaningful connections", [Node ("meaningful connections", [Node ("meaningful connections", [])])]);
                    Node ("meaningful connections", [Node ("\023", [Node ("meaningful connections", []); Node ("meaningful connections", [])])]);
                    Node
                    ("meaningful connections",
                    [Node ("meaningful connections", [Node ("\013", [Node ("meaningful connections", [Node ("meaningful connections", [])])])]);
                        Node ("meaningful connections", [])]); Node ("meaningful connections", []);
                    Node ("\009", [Node ("meaningful connections", []); Node ("\020", [])]);
                    Node ("meaningful connections", [Node ("\000", [])]);
                    Node
                    ("\021",
                    [Node ("meaningful connections", []); Node ("\003", [Node ("\029", [Node ("meaningful connections", [])])])]);
                    Node ("\026", []); Node ("meaningful connections", []);
                    Node ("meaningful connections", [Node ("meaningful connections", []); Node ("\007", [])]);
                    Node ("meaningful connections", [Node ("meaningful connections", [])]);
                    Node
                    ("meaningful connections",
                    [Node ("meaningful connections", [Node ("\002", [])]);
                        Node
                        ("meaningful connections",
                        [Node
                            ("meaningful connections",
                            [Node ("\012", []); Node ("\022", []);
                                Node ("meaningful connections", [Node ("meaningful connections", []); Node ("\b", [t3])])]);
                            Node ("\009", []); Node ("meaningful connections", []);
                            Node ("meaningful connections", [Node ("\005", [])])]); Node ("meaningful connections", []);
                        Node ("\026", [])]);
                    Node
                    ("meaningful connections",
                    [Node ("meaningful connections", []);
                        Node ("meaningful connections", [Node ("meaningful connections", [Node ("\000", [Node ("meaningful connections", [])])])])])]);
                Node ("\013", []); Node ("meaningful connections", []); Node ("\023", []); Node ("meaningful connections", []);
                Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []);
                Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", []);
                Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("\021", []); Node ("meaningful connections", []);
                Node ("\031", []); Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("\004", [])]);
            Node ("meaningful connections", [Node ("meaningful connections", [])]); Node ("meaningful connections", []);
            Node
            ("meaningful connections",
            [Node ("meaningful connections", [Node ("meaningful connections", [Node ("\010", [])]); Node ("meaningful connections", [])]);
                Node
                ("meaningful connections",
                [Node ("meaningful connections", []); Node ("\022", []); Node ("meaningful connections", []); Node ("meaningful connections", []);
                    Node ("\000", []); Node ("\014", []); Node ("meaningful connections", []); Node ("meaningful connections", []);
                    Node ("meaningful connections", []); Node ("meaningful connections", []);
                    Node
                    ("meaningful connections",
                    [Node
                        ("\002",
                        [Node ("meaningful connections", []);
                            Node ("meaningful connections", [Node ("meaningful connections", []); Node ("meaningful connections", [Node ("meaningful connections", [])])]);
                            Node ("meaningful connections", [Node ("\b", [])]); Node ("meaningful connections", []);
                            Node
                            ("\005",
                            [Node ("\b", [Node ("\015", []); Node ("meaningful connections", [])])])])]);
                    Node ("meaningful connections", []);
                    Node
                    ("meaningful connections",
                    [Node
                        ("meaningful connections",
                        [Node ("meaningful connections", []); Node ("\011", []); Node ("meaningful connections", []);
                            Node ("\021", []);
                            Node
                            ("meaningful connections",
                            [Node ("meaningful connections", [Node ("meaningful connections", [])]); Node ("meaningful connections", []);
                                Node ("meaningful connections", [Node ("meaningful connections", [])]); Node ("meaningful connections", []);
                                Node ("meaningful connections", [Node ("meaningful connections", [])]); Node ("meaningful connections", [])]);
                            Node ("\031", [])]);
                        Node
                        ("meaningful connections",
                        [Node ("meaningful connections", [Node ("meaningful connections", [])]); Node ("meaningful connections", []);
                            Node ("meaningful connections", [Node ("meaningful connections", [])]); Node ("meaningful connections", [])]);
                        Node
                        ("\005",
                        [Node ("\006", []); Node ("\004", []);
                            Node ("meaningful connections", [Node ("meaningful connections", [])]); Node ("\014", []);
                            Node ("meaningful connections", [])])]); Node ("meaningful connections", []); Node ("meaningful connections", [])]);
                Node ("meaningful connections", []); Node ("meaningful connections", []); Node ("meaningful connections", [])])])
 
    let t5 = Node("ratoootaattoo", 
        [Node("asdjlaksj", [Node("asjdla", [Node("lskadja i wish i wan the way iyam", []); Node("dsjal", [Node("das", [])])])]);
        Node("laras", [Node("lskjadoia", []); Node("i know u can heasd", []); Node("djalk", [Node("djaskl", []); Node("222daassdsad", [])])])])

    t5 |> generateChart 5.0 10 true |> showChart

    0
