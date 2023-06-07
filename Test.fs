module Test

open Tree
open Plot
open FsCheck

let leafGen = Gen.map (fun c -> Node(c, [])) Arb.generate<char>

let safeTreeGen () =
    let rec treeGen n =
        match n with
        | 0 -> leafGen
        | _ ->
            let bgen = branchesGen (n / 2)
            Gen.map2 (fun c ts -> Node(c, ts)) Arb.generate<char> bgen

    and branchesGen n = Gen.listOf (treeGen n) // random length. some length? and posibly empty. important. so leaf generator not that important...
    Gen.sized treeGen

let rec subTrees (Node(x, ts)) =
    (seq { yield! Seq.collect subTrees ts }, seq { yield Node(x, ts) })
    ||> Seq.append

type Generators =
    static member Tree() =
        { new Arbitrary<Tree<char>>() with
            override x.Generator = safeTreeGen ()
            override x.Shrinker t = subTrees t }

Arb.register<Generators> () |> ignore

let check f = Check.Quick f

let fitTest (t: Tree<char>) =
    let p (Node(_, subTrees)) =
        (2, getCoords subTrees)
        ||> List.windowed
        |> List.forall (fun ls ->
            let x1 = ls |> List.head |> fst
            let x2 = ls |> List.last |> fst
            abs (x1 - x2) >= 1.0)

    t |> design |> absTree |> getNodes |> List.forall p

let fitProperty = Prop.forAll Arb.from<Tree<char>> fitTest

let symmetryTest (t: Tree<char>) =
    let p (Node((_, (x, _)), subTrees)) =
        match subTrees with
        | [] -> true
        | [ s ] -> true
        | _ ->
            let x1 = subTrees |> List.head |> getCoord |> fst
            let x2 = subTrees |> List.last |> getCoord |> fst

            (x1 + x2) / 2.0 = x

    t |> design |> absTree |> getNodes |> List.forall p

let symmetryProperty = Prop.forAll Arb.from<Tree<char>> symmetryTest

let mirrorTest (t: Tree<char>) =

    let rec reflectPos (Node((v, (x: float)), subTrees)) =
        Node((v, -x), subTrees |> List.map reflectPos)

    let rec reflect (Node(v, subTrees)) =
        Node(v, (subTrees |> List.rev |> List.map reflect))

    design (reflect t) = reflect (reflectPos (design t))

let mirrorProperty = Prop.forAll Arb.from<Tree<char>> mirrorTest

let rec getX (Node((_, x), ts)) = x :: List.map getX' ts

and getX' ((Node((_, x), _))) = x

let rec getNodes ((Node(x, subTrees)) as t) =
    match subTrees with
    | [] -> []
    | _ -> t :: List.collect getNodes subTrees

let rec getTreeShape (Node(_, subTrees)) = Node(0, List.map getTreeShape subTrees)

let subTreeConsistencyTest (t: Tree<char>) =
    let rec p = function
        | [] -> true
        | [ _ ] -> true
        | (Node(_, [])) :: _ -> true
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

let subTreeConsistencyProperty =
    Prop.forAll Arb.from<Tree<char>> subTreeConsistencyTest

let rec encode t =
    match t with
    | Node(_, []) -> "()"
    | Node(_, ts) ->
        ts
        |> List.map encode
        |> List.sort
        |> String.concat ""
        |> (fun s -> "(" + s + ")")

let subTreeConsistencyEncode (t: Tree<char>) =
    let rec p = function
        | [] -> true
        | [ _ ] -> true
        | (Node(_, [])) :: _ -> true
        | t1 :: t2 :: tail ->
            match getX t1, getX t2 with
            | [], [] -> p (t2 :: tail)
            | [ _ ], [ _ ] -> p (t2 :: tail)
            | x :: xs, y :: ys -> xs = ys && p (t2 :: tail)
            | _ -> false

    t |> design |> getNodes |> List.groupBy encode |> List.map snd |> List.forall p

let subTreeConsistencyPropertyEncode =
    Prop.forAll Arb.from<Tree<char>> subTreeConsistencyEncode

// we say a tree of one node (is root and leaf) has a depth of 1
let rec depth (Node(_, ts)) = 1 + List.fold max 0 (List.map depth ts)

// list of number of "links" to traverse from each leaf to the root
let rec allDepths (Node(_, ts)) =
    match ts with 
    | [] -> [0]
    | ts' -> List.collect (fun t -> allDepths t |> List.map (fun x -> x + 1)) ts'

let mean (xs: int list) =
    xs
    |> List.fold (fun (sum, n) v -> sum + v, n + 1) (0, 0)
    |> (fun (sum, n) -> if n = 0 then (0, 1) else (sum, n))
    ||> (fun sum n -> float sum / float n)

let avgWalkLenToLeaf t = t |> allDepths |> mean 

let treeClassify (t: Tree<char>) =
    depth t >= 1
    |> Prop.classify (depth t = 1) "max depth 1"
    |> Prop.classify (depth t = 2) "max depth 2"
    |> Prop.classify (depth t = 3) "max depth 3"
    |> Prop.classify (depth t = 4) "max depth 4"
    |> Prop.classify (depth t > 4) "max depth > 4"
    |> Prop.classify (avgWalkLenToLeaf t > 1.0 && avgWalkLenToLeaf t <= 2.0) "1 < avg walk <= 2"
    |> Prop.classify (avgWalkLenToLeaf t > 2.0 && avgWalkLenToLeaf t <= 3.0) "2 < avg walk <= 3"
    |> Prop.classify (avgWalkLenToLeaf t > 3.0 && avgWalkLenToLeaf t <= 4.0) "3 < avg walk <= 4"
    |> Prop.classify (avgWalkLenToLeaf t > 4.0) "avg walk > 4" 