module Plot

open Plotly.NET
open Types

let combineMaps m1 m2 =
    (m2, m1)
    ||> Map.fold (fun m k v ->
        m
        |> Map.change k (fun x ->
            match x with
            | Some l -> Some(l @ [ v ])
            | None -> Some([ v ])))

let combineConcs l =
    l |> List.fold (fun s m -> combineMaps m s) Map.empty

let combineStates (states: State seq) =
    states |> List.ofSeq |> List.map (fun s -> s.concentrations) |> combineConcs

let concToYs m = m |> Map.toList

let xsys (states: State seq) =
    let ys = states |> combineStates |> concToYs
    let xs = [ 0 .. (List.head ys |> snd |> List.length) ]
    (xs, ys)

let xsysSelect (states: State seq) (selection: string list) =
    let ys =
        states
        |> combineStates
        |> concToYs
        |> List.filter (fun (k, v) -> selection |> List.contains k)

    let xs = [ 0 .. (List.head ys |> snd |> List.length) ]
    (xs, ys)

let line (xs: 'a list) (ys: 'b list) name =
    Chart.Line(xs, ys, Name = name)
    |> Chart.withLineStyle (Width = 2.0, Dash = StyleParam.DrawingStyle.Solid)

let pieceWiseLinear (xs: int list) (ys: float list) (name: string) =
    Chart.Scatter(xs, ys, mode = StyleParam.Mode.Lines, Name = name)
    |> Chart.withLineStyle (Shape = StyleParam.Shape.Hv)

let lines f xs (ls: (string * float list) list) =
    ls
    |> List.map (fun (name, ys) -> f xs ys name)
    |> Chart.combine
    |> Chart.withXAxisStyle ("iteration")
    |> Chart.withYAxisStyle ("concentration")

let genPlot f (states: State seq) = xsys states ||> lines f

let genPlotSelect f (selection: string list) (states: State seq) = xsysSelect states selection ||> lines f


let showPlot plot = plot |> Chart.show
