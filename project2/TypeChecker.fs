module TypeChecker

//open AST
open Types

let isDisjoint (s1: Set<'a>, s2: Set<'a>) = (s1, s2) ||> Set.intersect = Set.empty


let initConcs (concs: Concentration list) = concs |> List.map fst |> Set

let getFromCnd =
    function
    | IfGE cl -> cl
    | IfGT cl -> cl
    | IfEQ cl -> cl
    | IfLE cl -> cl
    | IfLT cl -> cl

let checkSameSpeciesComp crn : CRN_Error =
    let hasSameSpecies (Step cl) =
        cl
        |> List.fold
            (fun cmps cmd ->
                match cmd with
                | Comp (a, b) -> (a, b) :: cmps
                | _ -> cmps)
            []
        |> List.forall (fun (a, b) -> a = b)

    let hasError = List.forall hasSameSpecies crn.steps

    if hasError then Error SameSpeciesComparison else Ok()

let checkCondNoFlags crn =
    let rec hasNoFlags cs =
        match cs with
        | Ar (_) :: cs' -> hasNoFlags cs'
        | Cond (_) :: _ -> true // if a cmp had preceeded this would not match
        | _ -> false

    let hasError = crn.steps |> List.collect (fun (Step cl) -> cl) |> hasNoFlags

    if hasError then Error CondNoFlags else Ok()

let rec checkWriteTwice crn =
    let rec aux =
        function
        | Cond cnd -> cnd |> getFromCnd |> List.collect aux
        | Comp _ -> []
        | Ar a ->
            match a with
            | Ld (_, dst) -> [ dst ]
            | Add (_, _, dst) -> [ dst ]
            | Sub (_, _, dst) -> [ dst ]
            | Mul (_, _, dst) -> [ dst ]
            | Div (_, _, dst) -> [ dst ]
            | Sqrt (_, dst) -> [ dst ]


    let hasError =
        crn.steps
        |> List.forall (fun (Step cl) ->
            let destinations = List.collect aux cl
            cl = List.distinct cl)
        |> not

    if hasError then Error WriteTwice else Ok()

let checkReadWrite crn =
    let getRWArith a =
        match a with
        | Ld (src, dst) -> Set [ src ], Set [ dst ]
        | Add (src1, src2, dst) -> (Set [ src1; src2 ]), Set [ dst ]
        | Sub (src1, src2, dst) -> (Set [ src1; src2 ]), Set [ dst ]
        | Mul (src1, src2, dst) -> (Set [ src1; src2 ]), Set [ dst ]
        | Div (src1, src2, dst) -> (Set [ src1; src2 ]), Set [ dst ]
        | Sqrt (src, dst) -> Set [ src ], Set[dst]

    let getRWComp (s1, s2) = Set [ s1; s2 ], Set.empty<Species>

    let rec getRWC (cmd: Command) =
        match cmd with
        | Ar (a) -> getRWArith a
        | Comp (s1, s2) -> getRWComp (s1, s2)
        | Cond (c) -> getRWCond (c)

    and getRWCL (cl: Command list) =
        cl
        |> List.map getRWC
        |> List.fold (fun (f1, f2) (s1, s2) -> f1 |> Set.union s1, f2 |> Set.union s2) (Set.empty, Set.empty)

    and getRWCond (c: Conditional) =
        match c with
        | IfGT (cl) -> getRWCL cl
        | IfGE (cl) -> getRWCL cl
        | IfEQ (cl) -> getRWCL cl
        | IfLT (cl) -> getRWCL cl
        | IfLE (cl) -> getRWCL cl

    let getRWStep (Step (cl)) = getRWCL cl

    // Get read write set for a root list
    let getRWRL (sl: Step list) =
        sl |> List.map (fun (Step (cl)) -> getRWCL cl)

    match
        crn.steps
        |> getRWRL
        |> List.forall (fun (rSet, wSet) -> (rSet, wSet) |> isDisjoint)
    with
    | true -> Ok()
    | _ -> Error CycleConflict

let checkSrcOpNotDef crn =
    let rec aux (env: State) cl =
        match cl with
        | [] -> true
        | Cond c :: tail ->
            let cndCommand = getFromCnd c
            aux env tail
        | Comp (s1, s2) :: tail -> env.ContainsKey s1 && env.ContainsKey s2 && aux env tail
        | Ar a :: tail ->
            match a with
            | Ld (a, b) -> env.ContainsKey a && aux (Map.add b 0.0 env) tail
            | Add (a, b, c) -> env.ContainsKey a && env.ContainsKey b && aux (Map.add c 0.0 env) tail
            | Sub (a, b, c) -> env.ContainsKey a && env.ContainsKey b && aux (Map.add c 0.0 env) tail
            | Mul (a, b, c) -> env.ContainsKey a && env.ContainsKey b && aux (Map.add c 0.0 env) tail
            | Div (a, b, c) -> env.ContainsKey a && env.ContainsKey b && aux (Map.add c 0.0 env) tail
            | Sqrt (a, _) -> env.ContainsKey a && aux env tail


    let hasError =
        crn.steps
        |> List.map (fun (Step cl) -> cl)
        |> List.forall (aux crn.molecules)
        |> not

    if hasError then Error SrcOpNotDef else Ok()

let checkCRN crn : CRN_Error =
    result {
        let! _ = checkSameSpeciesComp crn
        let! _ = checkCondNoFlags crn
        let! _ = checkWriteTwice crn
        let! _ = checkReadWrite crn
        let! _ = checkSrcOpNotDef crn

        return ()
    }
