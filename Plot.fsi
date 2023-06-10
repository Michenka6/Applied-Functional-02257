module Plot
open Tree
open Plotly.NET
open Plotly.NET.LayoutObjects // this namespace contains all object abstractions for layout styling

type Plot
val getCoord :Tree<'a * (float*float)> -> float * float
val getCoords : Tree<'a * (float*float)> -> (float * float) list
val getCoordsList : Tree<'a * (float*float)> list -> (float * float) list 
val absTree : Tree<'a * float> -> Tree<'a * (float * float)>
val treeToPoints : Tree<'a * (float * float)> -> GenericChart.GenericChart list
val verticalLines : Tree<'a * (float * float)> -> GenericChart.GenericChart list
val horizontalLines : Tree<'a * (float * float)> -> GenericChart.GenericChart list
val pointsAndLines : Tree<'a * (float * float)> -> GenericChart.GenericChart list
val generateChart : Tree<'a> -> GenericChart.GenericChart
val showChart : GenericChart.GenericChart -> unit 

