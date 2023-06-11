module TreeDrawing

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
                )) 
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
let rec annotations (firstn: int option) (hover: bool) (Node((label, (x, y)), subTrees)) =
    let labelText = if firstn.IsSome then (string label).[0..firstn.Value-1] else string label

    Annotation.init (X = x, Y = y, Text = labelText, HoverText = string label, ShowArrow = false, CaptureEvents = hover)
    :: List.collect (annotations firstn hover) subTrees

let layout (firstn : int option) hover fontSize t =
    Layout.init (
        Annotations = annotations firstn hover t,
        Font = Font.init (StyleParam.FontFamily.Arial, fontSize, Color.fromString "black"),
        AutoSize = true,
        PaperBGColor = Color.fromString "white",
        PlotBGColor = Color.fromHex "0xE5ECF6"
    )

let pointsAndLines factor (t: Tree<'a * (float * float)>) =
    verticalLines factor t @ horizontalLines factor t


type TreeDrawing = TD of GenericChart.GenericChart

type TreeDrawing with
    static member generateDrawing(t, ?scale: float, ?firstn: int, ?hover: bool, ?fontSize: float) =
        let factor = defaultArg scale 1.0
        let hover = defaultArg hover false
        let absT = t |> design |> absTree |> scaleAbsTree factor
        let size = defaultArg fontSize 12.0
        let chart =
            absT
            |> pointsAndLines factor
            |> Chart.combine
            |> GenericChart.setLayout (layout firstn hover size absT )

        let xAxis = LinearAxis.init (Visible = false, Mirror = StyleParam.Mirror.True)
        //let ys = getCoords absT |> List.map (fun (x,y) -> y)
        //let yAxis = LinearAxis.init (Visible = false, Range=StyleParam.Range.ofMinMax((List.min ys) - 10.0*factor, (List.max ys) + 10.0*factor))

        chart |> Chart.withXAxis xAxis |> Chart.withYAxis xAxis |> TD

let saveDrawing (path: string) (TD(c)) =
   c |> Chart.saveHtml (path, OpenInBrowser = false)

let showDrawing ((TD(c): TreeDrawing)) = c |> Chart.show
