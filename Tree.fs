module Tree

type Tree<'a> = Node of 'a * Branches<'a>
and Branches<'a> = Tree<'a> list

type PTree<'a> = Tree<'a * float>

type Extent = (float * float) list

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

let rec getNodes (Node (x, subTrees)) =
    (Node(x, subTrees)) :: List.collect getNodes subTrees
