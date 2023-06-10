module Plot

open Tree
open Plotly.NET
open Plotly.NET.LayoutObjects // this namespace contains all object abstractions for layout styling

let rec getCoords (Node((_, ((x: float), (y: float))), subTrees)) =
    (x, y) :: List.collect getCoords subTrees

let rec getCoordsList (subTrees: Tree<'a * (float * float)> list) =
    match subTrees with
    | [] -> []
    | Node((_, (x, y)), _) :: rest -> (x, y) :: getCoordsList rest

let rec getLabels (Node((label, _), subTrees)) =
    label :: List.collect getLabels subTrees

let absTree (Node((label, x), subTrees)) =
    let rec aux (prevX: float) (y: float) (Node((label, x), subTrees)) =
        Node((label, ((prevX + x), (y))), List.map (aux (prevX + x) (y - 1.0)) subTrees) in

    aux 0 0 (Node((label, x), subTrees))

let rec scalePTree (factor: float) (Node((l, x: float), ts)) =
    Node((l, x * factor), List.map (scalePTree factor) ts)

let rec scaleAbsTree (factor: float) (Node((l, (x: float, y: float)), ts)) =
    Node((l, (x * factor, y * factor)), List.map (scaleAbsTree factor) ts)

let rec treeToPoints (Node((label: 'a, (x: float, y: float)), subTrees)) =
    Chart.Point(
        [ (x, y) ],
        MultiText = [ string label ],
        MultiTextPosition = [ StyleParam.TextPosition.Inside ],
        ShowLegend = false,
        MarkerColor = (Color.fromARGB 0 0 0 0)
    )
    :: List.collect treeToPoints subTrees

let rec treeToPoints1 t =
    Chart.Point(
        getCoords t,
        ShowLegend = false,
        MarkerColor = (Color.fromARGB 0 0 0 0)
    )

let rec verticalLines (factor: float) (Node((_, ((x: float), (y: float))), subTrees)) =
    match subTrees with
    | [] -> []
    | _ ->
        Chart.Line(
            [ x; x ],
            [ y - (0.150 * factor); y - factor / 2.0 ],
            LineColor = Color.fromString "black",
            ShowLegend = false
        )
        :: List.map
            (fun (Node((_, (x, y)), _)) ->
                Chart.Line(
                    [ x; x ],
                    [ y + (0.150 * factor); y + (factor / 2.0) ],
                    LineColor = Color.fromString "black",
                    ShowLegend = false
                )) // -(y/2.0) because y will be negative here. extend line from node and up
            subTrees
        @ List.collect (verticalLines factor) subTrees

let rec horizontalLines (factor: float) (Node((_, (_, _)), subTrees)) =
    match subTrees |> getCoordsList with
    | [] -> []
    | (_, _) :: [] -> List.collect (horizontalLines factor) subTrees
    | tup :: rest ->
        Chart.Line(
            [ fst tup; fst (rest |> List.last) ],
            [ snd tup + factor / 2.0; snd tup + factor / 2.0 ], // snd tup is negatve. pre condition ...
            LineColor = Color.fromString "black",
            ShowLegend = false
        )
        :: List.collect (horizontalLines factor) subTrees

let rec annotations (Node((label, (x, y)), subTrees)) =

    let labelText = (string label |> List.ofSeq |> List.head |> string)

    Annotation.init (X = x, Y = y, Text = labelText, HoverText = string label, ShowArrow = false, CaptureEvents = true)
    :: List.collect annotations subTrees

let layout t =
    Layout.init (
        Annotations = annotations t,
        Font = Font.init (StyleParam.FontFamily.Arial, 12.0, Color.fromString "black"),
        AutoSize = true,
        PaperBGColor = Color.fromString "white",
        PlotBGColor = Color.fromHex "0xE5ECF6"
    )

let pointsAndLines factor (t: Tree<'a * (float * float)>) =
    verticalLines factor t @ horizontalLines factor t

let generateChart factor t =
    let absT = t |> design |> absTree |> scaleAbsTree factor

    let chart =
        absT
        |> pointsAndLines factor
        |> Chart.combine
        |> GenericChart.setLayout (layout absT)

    let xAxis = LinearAxis.init (Visible = false, Mirror = StyleParam.Mirror.True)

    chart |> Chart.withXAxis xAxis |> Chart.withYAxis xAxis

let saveChart (path: string) (c: GenericChart.GenericChart) =
    Chart.saveHtml (path, OpenInBrowser = false)

let showChart (c: GenericChart.GenericChart) = c |> Chart.show
