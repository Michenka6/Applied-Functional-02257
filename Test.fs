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

            if x1 + x2 = x then
                true
            else
                printfn $"x1: {x1} x2: {x2}"
                printfn $"x : {x}"
                false

    t |> design |> absTree |> getNodes |> List.forall p
