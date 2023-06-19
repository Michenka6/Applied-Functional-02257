module Plot

open Plotly.NET
open Types 

let plot = 

    let x  = [1952; 1957; 1962; 1967; 1972; 1977; 1982; 1987; 1992; 1997; 2002; 2007]

    // Australia
    let y1 = [69.12; 70.30; 70.93; 71.1; 71.93; 73.49; 74.74; 76.32; 77.56; 78.3; 80.37; 81.235]
    // New Zealand
    let y2 = [69.39; 70.26; 71.24; 71.52; 71.89; 72.22; 73.84; 74.32; 76.33; 77.55; 79.11; 80.204]

    [
            Chart.Line(x, y1, Name="Australia") 
            |> Chart.withLineStyle(Width=2.0, Dash=StyleParam.DrawingStyle.Solid)

            Chart.Line(x, y2, Name="New Zealand")
            //|> Chart.withTraceName(Name="New Zealand")
            |> Chart.withLineStyle(Width=2.0, Dash=StyleParam.DrawingStyle.Solid) 
    ] 
    |> Chart.combine
    |> Chart.withXAxisStyle("year")
    |> Chart.withYAxisStyle("lifeExp")

let combineMaps m1 m2 = 
    (m2, m1) 
    ||> Map.fold (fun m k v -> 
        m |> Map.change k (fun x -> 
            match x with 
            | Some l -> Some (l @ [v])
            | None -> Some([v]))) 

(* let stateToYs (states: State seq) = 
    states 
    |> List.ofSeq
    |> List.map (fun s -> s.concentrations)
    |> List.map Map.toList
    |> List.collect id 
    |> List.groupBy (fun (k, v) -> k)
    |> List.map (fun (k, v) -> v)
    |> List.rev // rev to use cons.  
    |> List.map (fun l -> l |> List.fold (fun (key, l) (k,v) -> (k, v::l)) ("", []) )
 *)

let combineConcs l =
    l 
    |> List.fold  (fun s m -> combineMaps m s) Map.empty

let combineStates (states: State seq) = 
    states 
    |> List.ofSeq
    |> List.map (fun s -> s.concentrations)
    |> combineConcs

let concToYs m = 
    m 
    |> Map.toList

let xsys (states: State seq) = 
    let ys = states |> combineStates |> concToYs
    let xs = [0 .. (List.head ys |> snd |> List.length)]
    (xs, ys)

let xsysSelect (states: State seq) (selection: string list) = 
    let ys = states |> combineStates |> concToYs |> List.filter (fun (k,v) -> selection |> List.contains k)
    let xs = [0 .. (List.head ys |> snd |> List.length)]
    (xs, ys)

let line (xs: 'a list) (ys: 'b list) name = 
    Chart.Line(xs, ys, Name=name) 
    |> Chart.withLineStyle(Width=2.0, Dash=StyleParam.DrawingStyle.Solid)

let pieceWiseLinear (xs :int list) (ys: float list) (name: string) = 
    Chart.Scatter(xs, ys, mode=StyleParam.Mode.Lines, Name=name) 
    |> Chart.withLineStyle(Shape=StyleParam.Shape.Hv)

let lines f xs (ls : (string * float list) list) = 
    ls 
    |> List.map (fun (name, ys) -> f xs ys name)
    |> Chart.combine
    |> Chart.withXAxisStyle("iteration")
    |> Chart.withYAxisStyle("concentration")


let genPlot f (states: State seq) =
    //let ys = states |> combineStates |> concToYs
    //let xs = [0 .. (List.head ys |> snd |> List.length)]
    //let xs, ys = xsys states
    //lines f xs ys 
    xsys states ||> lines f

let genPlotSelect f (selection: string list) (states: State seq) =
    xsysSelect states selection ||> lines f 


let showPlot plot = 
    plot |> Chart.show