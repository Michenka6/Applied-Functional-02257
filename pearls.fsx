type Tree<'a> = Node of 'a * Branches<'a>
and Branches<'a> = Tree<'a> list

type PTree<'a> = Tree<'a * float>

type Extent = (float * float) list

let d = Node("D", [])
let c = Node("C", [d; d])
//let c = Node("C", [])
let b = Node("B", [ c ; c; c])
let a = Node("A", [ d; b; c; d])
let t = Node("T", [ a ])
//let t = Node("T", [c;c;d])

let moveTree (Node ((label, x), subTrees): PTree<'a>, x': float) = Node((label, x + x'), subTrees)

let moveExtent (e: Extent, x) =
    List.map (fun (a, b) -> (a + x, b + x)) e

let rec merge ps qs =
    match ps, qs with
    | [], _ -> qs
    | _, [] -> ps
    | (x, _) :: xs, (_, y) :: ys -> (x, y) :: merge xs ys

let mergeList (es: ('a * 'b) list list) = List.fold merge [] es

let rMax p q = max p q

let rec fit ps qs =
    match ps, qs with
    | (_, x) :: xs, (y, _) :: ys -> rMax (fit xs ys) (x - y + 1.0)
    | _, _ -> 0.0

let fitListL es =
    let rec aux acc =
        function
        | [] -> []
        | e :: es ->
            let x = fit acc e
            x :: aux (merge acc (moveExtent (e, x))) es

    aux [] es

let fitListR es =
    let rec aux acc =
        function
        | [] -> []
        | e :: es ->
            let x = -fit e acc
            x :: aux (merge (moveExtent (e, x)) acc) es

    ([], List.rev es) ||> aux |> List.rev

let flipExtent = List.map (fun (p, q) -> (-q, -p))

let mean (x, y) = (x + y) / 2.0

let fitList es =
    (fitListL es, fitListR es) ||> List.zip |> List.map mean

let rec design' (Node (label, subTrees)) =
    let (trees, extents) = subTrees |> List.map design' |> List.unzip
    let positions = fitList extents
    let pTrees = (trees, positions) ||> List.zip |> List.map moveTree
    let pExtents = (extents, positions) ||> List.zip |> List.map moveExtent
    let resultExtent = (0.0, 0.0) :: mergeList pExtents
    let resultTree = Node((label, 0.0), pTrees)

    resultTree, resultExtent

let design tree = fst (design' tree)
let design1 tree = snd (design' tree)

// Michael R. Hansen    30-05-2023
#r "nuget: Plotly.NET, 4.0.0"


open Plotly.NET
open System


open Plotly.NET.LayoutObjects // this namespace contains all object abstractions for layout styling

let mirroredXAxis =
    LinearAxis.init (
        //        Title = Title.init(Text="Mirrored axis"),
        ShowLine = true,
        Mirror = StyleParam.Mirror.False,
        ShowGrid = true
    //        Ticks = StyleParam.TickOptions.Inside
    )

let mirroredYAxis =
    LinearAxis.init (
        //        Title = Title.init(Text="Log axis"),
        //        AxisType = StyleParam.AxisType.Log,
        ShowLine = true,
        Mirror = StyleParam.Mirror.False,
        ShowGrid = true
    )

let rec addVerticals acc (Node ((label, x), subTrees)) =
    Node((label, (x, acc)), List.map (addVerticals (acc - 1.0)) subTrees)

let absTree (Node ((label, x), subTrees)) = 
    let rec absTree_ (prevX: float) (y: float) (Node ((label, x), subTrees)) = 
        Node((label, ((prevX + x), (y))), List.map (absTree_ (prevX + x) (y - 1.0)) subTrees)
    in absTree_ 0 0 (Node ((label, x), subTrees)) 
let rec treeToPoints (Node ((label, (x, y)), subTrees)) =
    Chart.Point(
        [ (x, y) ],
        MultiText = [ label ],
        MultiTextPosition = [ StyleParam.TextPosition.TopCenter ],
        ShowLegend = true
    )
    :: List.collect treeToPoints subTrees
let rec verticalLines (Node((label, (x, y)), subTrees)) = 
    match subTrees with 
    | [] -> [] 
    | _ -> 
        Chart.Line([ x; x ], [ y; y-1.0 ], 
        LineColor = Color.fromString "black", 
        ShowLegend = false) :: List.collect verticalLines subTrees
let rec getCoords subTrees =  
    match subTrees with 
    | [] -> []
    | Node((label, (x, y)), _)::rest -> (x,y)::getCoords rest 

let rec horizontalLines (Node((label, (x, y)), subTrees)) = 
    match subTrees |> getCoords with 
    | [] -> []
    | (x, y)::[] -> List.collect horizontalLines subTrees  
    | tup::rest ->  Chart.Line([ fst tup ; fst (rest |> List.last) ], 
                    [ snd tup; snd tup ], 
                    LineColor = Color.fromString "black", 
                    ShowLegend = false)  :: List.collect horizontalLines subTrees
    
let pointsAndLines (t, e) = 
    t 
    |> absTree 
    |> (fun t -> (treeToPoints t) @ (verticalLines t) @ (horizontalLines t)) 

let test t =
    t
    |> design
    |> addVerticals 0.0
    |> treeToPoints
    |> Chart.combine
    |> Chart.withXAxis mirroredXAxis
    |> Chart.withYAxis mirroredYAxis
    |> Chart.show

let test1 t =     
    t
    |> design
    |> addVerticals 0
    //|> treeToPoints 
    |> printfn "%A" 

let test2 t = 
    t
    |> design 
    |> absTree 
    |> treeToPoints
    |> Chart.combine
    |> Chart.withXAxis mirroredXAxis
    |> Chart.withYAxis mirroredYAxis
    |> Chart.show 
    
let test3 t = 
    t
    |> design'
    |> pointsAndLines  
    |> Chart.combine
    |> Chart.withXAxis mirroredXAxis
    |> Chart.withYAxis mirroredYAxis
    |> Chart.show 
 

//test1 t;;
//test t ;;
//design1 t |> printfn "%A"
//test2 t;;
test3 t ;;