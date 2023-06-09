module Plot

open Tree
open Plotly.NET
open Plotly.NET.LayoutObjects // this namespace contains all object abstractions for layout styling

type Plot = P
let absTree (Node ((label, x), subTrees)) =
    let rec aux (prevX: float) (y: float) (Node ((label, x), subTrees)) =
        Node((label, ((prevX + x), (y))), List.map (aux (prevX + x) (y - 1.0)) subTrees) in

    aux 0 0 (Node((label, x), subTrees))

let rec treeToPoints (Node ((label : 'a, (x: float, y: float)), subTrees)) =
    Chart.Point(
        [ (x, y) ],
        MultiText = [ string label ],
        MultiTextPosition = [ StyleParam.TextPosition.TopRight ],
        ShowLegend = true
    )
    :: List.collect treeToPoints subTrees

let rec verticalLines (Node ((_, ((x: float), (y: float))), subTrees)) =
    match subTrees with
    | [] -> []
    | _ ->
        Chart.Line([ x; x ], [ y; y - 0.5 ], LineColor = Color.fromString "black", ShowLegend = false)
        :: List.map
            (fun (Node ((_, (x, y)), _)) ->
                Chart.Line([ x; x ], [ y; y + 0.5 ], LineColor = Color.fromString "black", ShowLegend = false))
            subTrees
        @ List.collect verticalLines subTrees

let getCoord (Node ((_, ((x: float), (y: float))), _)) = x, y

let rec getCoords (subTrees: Tree<'a * (float * float)> list) =
    match subTrees with
    | [] -> []
    | Node ((_, (x, y)), _) :: rest -> (x, y) :: getCoords rest

let rec horizontalLines (Node ((_, (_, _)), subTrees)) =
    match subTrees |> getCoords with
    | [] -> []
    | (_, _) :: [] -> List.collect horizontalLines subTrees
    | tup :: rest ->
        Chart.Line(
            [ fst tup; fst (rest |> List.last) ],
            [ snd tup + 0.5; snd tup + 0.5 ],
            LineColor = Color.fromString "black",
            ShowLegend = false
        )
        :: List.collect horizontalLines subTrees

let pointsAndLines (t: Tree<'a * (float * float)>) =
    treeToPoints t @ verticalLines t @ horizontalLines t

let plot t = 
    t
    |> design
    |> absTree
    |> pointsAndLines
    |> Chart.combine
    |> Chart.show
