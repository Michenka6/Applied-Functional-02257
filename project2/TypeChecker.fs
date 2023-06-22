module TypeChecker

//open AST
open Types

let isDisjoint (s1: Set<'a>, s2: Set<'a>) = (s1, s2) ||> Set.intersect = Set.empty

let initConcs (concs: ConcList) =
    let m = Map [ ("Xgty", 0.0); ("Xlty", 0.0); ("Ygtx", 0.0); ("Yltx", 0.0) ]
    concs |> List.fold (fun env (Cnc ((s), n)) -> env |> Map.add s n) m

let getFromCnd =
    function
    | IfGE cl -> cl
    | IfGT cl -> cl
    | IfEQ cl -> cl
    | IfLE cl -> cl
    | IfLT cl -> cl

let checkSameSpeciesComp (Crn (c, s)) : CRN_Error =
    let hasSameSpecies (Stp cl) =
        cl
        |> List.fold
            (fun cmps cmd ->
                match cmd with
                | Comp (Cmp (a, b)) -> (a, b) :: cmps
                | _ -> cmps)
            []
        |> List.forall (fun (a, b) -> a = b)

    let hasError = List.forall hasSameSpecies s

    if hasError then Error SameSpeciesComparison else Ok()

let checkCondNoFlags (Crn (c, s)) =
    let rec hasNoFlags cs =
        match cs with
        | Ar (_) :: cs' -> hasNoFlags cs'
        | Cond (_) :: _ -> true // if a cmp had preceeded this would not match
        | _ -> false

    let hasError = s |> List.collect (fun (Stp cl) -> cl) |> hasNoFlags

    if hasError then Error CondNoFlags else Ok()

let rec checkWriteTwice (Crn (c, s)) =
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
        s
        |> List.forall (fun (Stp cl) ->
            let destinations = List.collect aux cl
            cl = List.distinct cl)
        |> not

    if hasError then Error WriteTwice else Ok()

let checkReadWrite (Crn (c, s)) =
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
        | Comp (Cmp (s1, s2)) -> getRWComp (s1, s2)
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

    let getRWStp (Stp (cl)) = getRWCL cl

    // Get read write set for a root list
    let getRWRL (sl: Step list) =
        sl |> List.map (fun (Stp (cl)) -> getRWCL cl)

    match s |> getRWRL |> List.forall (fun (rSet, wSet) -> (rSet, wSet) |> isDisjoint) with
    | true -> Ok()
    | _ -> Error CycleConflict

let checkSrcOpNotDef (Crn (c, s)) =
    let rec aux (env: State) cl =
        match cl with
        | [] -> true
        | Cond c :: tail ->
            let cndCommand = getFromCnd c
            aux env tail
        | Comp (Cmp (s1, s2)) :: tail ->
            env.concentrations.ContainsKey s1
            && env.concentrations.ContainsKey s2
            && aux env tail
        | Ar a :: tail ->
            match a with
            | Ld (a, b) ->
                env.concentrations.ContainsKey a
                && aux ({ env with concentrations = Map.add b 0.0 env.concentrations }) tail
            | Add (a, b, c) ->
                env.concentrations.ContainsKey a
                && env.concentrations.ContainsKey b
                && aux ({ env with concentrations = Map.add c 0.0 env.concentrations }) tail
            | Sub (a, b, c) ->
                env.concentrations.ContainsKey a
                && env.concentrations.ContainsKey b
                && aux ({ env with concentrations = Map.add c 0.0 env.concentrations }) tail
            | Mul (a, b, c) ->
                env.concentrations.ContainsKey a
                && env.concentrations.ContainsKey b
                && aux ({ env with concentrations = Map.add c 0.0 env.concentrations }) tail
            | Div (a, b, c) ->
                env.concentrations.ContainsKey a
                && env.concentrations.ContainsKey b
                && aux ({ env with concentrations = Map.add c 0.0 env.concentrations }) tail
            | Sqrt (a, _) -> env.concentrations.ContainsKey a && aux env tail


    let hasError =
        s
        |> List.map (fun (Stp cl) -> cl)
        |> List.forall (
            aux
                { status = Running
                  concentrations = initConcs c }
        )
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
