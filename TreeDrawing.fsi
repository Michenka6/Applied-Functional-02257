module TreeDrawing
open Tree
open Plotly.NET
open Plotly.NET.LayoutObjects // this namespace contains all object abstractions for layout styling

[<Sealed>]
type TreeDrawing =
    static member generateDrawing : Tree<'a> * ?scale:float * ?firstn:int * ?hover:bool -> GenericChart.GenericChart
    
val showDrawing : GenericChart.GenericChart -> unit
val saveDrawing : string -> GenericChart.GenericChart  -> (GenericChart.GenericChart -> unit)
val absTree : Tree<'a * float> -> Tree<'a * (float * float)>

