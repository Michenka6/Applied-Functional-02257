module TreeDrawing
open Tree

[<Sealed>]
type TreeDrawing =
    static member generateDrawing : Tree<'a> * ?scale:float * ?firstn:int * ?hover:bool * ?fontSize:float -> TreeDrawing
    
val showDrawing : TreeDrawing -> unit
val saveDrawing : string -> TreeDrawing  -> unit
val absTree : Tree<'a * float> -> Tree<'a * (float * float)>

