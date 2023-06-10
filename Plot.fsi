module Plot
open Tree
open Plotly.NET
open Plotly.NET.LayoutObjects // this namespace contains all object abstractions for layout styling

val absTree : Tree<'a * float> -> Tree<'a * (float * float)>
val generateChart : float -> Tree<'a> -> GenericChart.GenericChart
val showChart : GenericChart.GenericChart -> unit
val saveChart : string -> GenericChart.GenericChart  -> (GenericChart.GenericChart -> unit)
