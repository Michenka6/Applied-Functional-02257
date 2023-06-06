module Test

open Tree
open Plot
open FsCheck


let check f = Check.Quick f


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

let getTree n label =
    Node(label, List.replicate n (Node(label, [])))

let rec getX (Node ((label, x), ts)) = x :: List.collect getX ts

let subTreeConsistencyTest (l1: char) (l2: char) (n: int) =
    let m = (abs n) % 2
    let ls1 = (m, l1) ||> getTree |> design |> getX
    let ls2 = (m, l2) ||> getTree |> design |> getX
    ls1 = ls2
