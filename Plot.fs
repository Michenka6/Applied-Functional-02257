module Plot

open Tree
open Plotly.NET
open System


open Plotly.NET.LayoutObjects // this namespace contains all object abstractions for layout styling

// let mirroredXAxis =
//     LinearAxis.init (
//         //        Title = Title.init(Text="Mirrored axis"),
//         ShowLine = true,
//         Mirror = StyleParam.Mirror.False,
//         ShowGrid = true
//     //        Ticks = StyleParam.TickOptions.Inside
//     )

// let mirroredYAxis =
//     LinearAxis.init (
//         //        Title = Title.init(Text="Log axis"),
//         //        AxisType = StyleParam.AxisType.Log,
//         ShowLine = true,
//         Mirror = StyleParam.Mirror.False,
//         ShowGrid = true
//     )

let rec addVerticals acc (Node ((label, x), subTrees)) =
    Node((label, (x, acc)), List.map (addVerticals (acc - 1.0)) subTrees)

let absTree (Node ((label, x), subTrees)) =
    let rec aux (prevX: float) (y: float) (Node ((label, x), subTrees)) =
        Node((label, ((prevX + x), (y))), List.map (aux (prevX + x) (y - 1.0)) subTrees) in

    aux 0 0 (Node((label, x), subTrees))

let rec treeToPoints (Node ((label, (x, y)), subTrees)) =
    Chart.Point(
        [ (x, y) ],
        MultiText = [ label ],
        MultiTextPosition = [ StyleParam.TextPosition.TopRight ],
        ShowLegend = true
    )
    :: List.collect treeToPoints subTrees

let rec verticalLines (Node ((label, (x, y)), subTrees)) =
    match subTrees with
    | [] -> []
    | _ ->
        Chart.Line([ x; x ], [ y; y - 0.5 ], LineColor = Color.fromString "black", ShowLegend = false)
        :: List.map
            (fun (Node ((_, (x, y)), _)) ->
                Chart.Line([ x; x ], [ y; y + 0.5 ], LineColor = Color.fromString "black", ShowLegend = false))
            subTrees
        @ List.collect verticalLines subTrees

let rec getCoords subTrees =
    match subTrees with
    | [] -> []
    | Node ((label, (x, y)), _) :: rest -> (x, y) :: getCoords rest

let rec horizontalLines (Node ((label, (x, y)), subTrees)) =
    match subTrees |> getCoords with
    | [] -> []
    | (x, y) :: [] -> List.collect horizontalLines subTrees
    | tup :: rest ->
        Chart.Line(
            [ fst tup; fst (rest |> List.last) ],
            [ snd tup + 0.5; snd tup + 0.5 ],
            LineColor = Color.fromString "black",
            ShowLegend = false
        )
        :: List.collect horizontalLines subTrees

let pointsAndLines t =
    treeToPoints t @ verticalLines t @ horizontalLines t

let plot t =
    t
    |> design
    |> absTree
    |> pointsAndLines
    |> Chart.combine
    // |> Chart.withXAxis mirroredXAxis
    // |> Chart.withYAxis mirroredYAxis
    |> Chart.show
