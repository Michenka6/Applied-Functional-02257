module Plot
open Tree
open Plotly.NET
open Plotly.NET.LayoutObjects // this namespace contains all object abstractions for layout styling

[<Sealed>]
type Plot =
    static member generateChart : Tree<'a> * ?scale:float * ?firstn:int * ?hover:bool -> GenericChart.GenericChart
    
val showChart : GenericChart.GenericChart -> unit
val saveChart : string -> GenericChart.GenericChart  -> (GenericChart.GenericChart -> unit)
val absTree : Tree<'a * float> -> Tree<'a * (float * float)>

