module Test

open Tree
open Plot
open FsCheck


let check f = Check.Verbose f


let fitTest (t: Tree<char>) =
    let newTree = t |> design |> absTree

    let p (Node ((label, (x, y)), subTrees)) =
        (2, getCoords subTrees)
        ||> List.windowed
        |> List.forall (fun ls ->
            let x1 = ls |> List.head |> fst
            let x2 = ls |> List.last |> fst
            abs (x1 - x2) >= 1)

    newTree |> getNodes |> List.forall p
