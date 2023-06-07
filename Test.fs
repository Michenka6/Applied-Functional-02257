module Test

open Tree
open Plot
open FsCheck

let leafGen = Gen.map (fun c -> Node(c, [])) Arb.generate<char>

let safeTreeGen() = 
    let rec treeGen n = match n with 
                        | 0 ->  leafGen
                        | _ ->  let bgen = branchesGen (n / 2) 
                                Gen.map2 (fun c ts -> Node(c, ts)) Arb.generate<char> bgen 
    and branchesGen n = Gen.listOf (treeGen n) // random length. some length? and posibly empty. important.
    Gen.sized treeGen ;;

let rec subTrees (Node(x, ts)) = (seq {yield! Seq.collect subTrees ts}, seq {yield Node(x, ts)} ) ||> Seq.append

type Generators = 
    static member Tree() = 
        {new Arbitrary<Tree<char>>() with 
                override x.Generator = safeTreeGen()
                override x.Shrinker t = subTrees t
        }

Arb.register<Generators>()

let check f = Check.Verbose f

let fitTest (t: Tree<char>) =
    let p (Node (_, subTrees)) =
        (2, getCoords subTrees)
        ||> List.windowed
        |> List.forall (fun ls ->
            let x1 = ls |> List.head |> fst
            let x2 = ls |> List.last |> fst
            abs (x1 - x2) >= 1.0)

    t |> design |> absTree |> getNodes |> List.forall p

let symmetryTest (t: Tree<char>) =
    let p (Node ((_, (x, _)), subTrees)) =
        match subTrees with
        | [] -> true
        | [ s ] -> true
        | _ ->
            let x1 = subTrees |> List.head |> getCoord |> fst
            let x2 = subTrees |> List.last |> getCoord |> fst

            (x1 + x2) / 2.0 = x

    t |> design |> absTree |> getNodes |> List.forall p

let mirrorTest (t: Tree<char>) =

    let rec reflectPos (Node ((v, (x: float)), subTrees)) =
        Node((v, -x), subTrees |> List.map reflectPos)

    let rec reflect (Node (v, subTrees)) =
        Node(v, (subTrees |> List.rev |> List.map reflect))

    design (reflect t) = reflect (reflectPos (design t))


let rec getX (Node ((_, x), ts)) = x :: List.map getX' ts

and getX' ((Node ((_, x), _))) = x


let rec getNodes ((Node (x, subTrees)) as t) =
    match subTrees with
    | [] -> []
    | _ -> t :: List.collect getNodes subTrees

let rec getTreeShape (Node (_, subTrees)) = Node(0, List.map getTreeShape subTrees)

let subTreeConsistencyTest (t: Tree<char>) =
    let rec p =
        function
        | [] -> true
        | [ _ ] -> true
        | (Node (_, [])) :: _ -> true
        | t1 :: t2 :: tail ->
            match getX t1, getX t2 with
            | [], [] -> p (t2 :: tail)
            | [ _ ], [ _ ] -> p (t2 :: tail)
            | x :: xs, y :: ys -> xs = ys && p (t2 :: tail)
            | _ -> false

    t
    |> design
    |> getNodes
    |> List.groupBy getTreeShape
    |> List.map snd
    |> List.forall p

let rec encode t =
    match t with
    | Node(_, []) -> "()"
    | Node(_, ts) ->
        ts
        |> List.map encode
        |> List.sort
        |> String.concat ""
        |> (fun s -> "(" + s + ")")

let subTreeConsistencyTest1 (t: Tree<char>) =  
    let rec p =
        function
        | [] -> true
        | [ _ ] -> true
        | (Node (_, [])) :: _ -> true
        | t1 :: t2 :: tail ->
            match getX t1, getX t2 with 
            | [], [] -> p (t2 :: tail)
            | [ _ ], [ _ ] -> p (t2 :: tail)
            | x :: xs, y :: ys -> xs = ys && p (t2 :: tail)
            | _ -> false 
    
    t
    |> design   
    |> getNodes 
    |> List.groupBy encode 
    |> List.map snd
    |> List.forall p

let subTreeConsistencyTest2 = 
    Prop.forAll Arb.from<Tree<char>> subTreeConsistencyTest1 