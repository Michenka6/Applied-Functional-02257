module TypeChecker

open FParsec
//open AST
open Types
open Parser

let isDisjoint (s1: Set<'a>, s2: Set<'a>) = (s1, s2) ||> Set.intersect = Set.empty


let initConcs (concs: Concentration list) = concs |> List.map fst |> Set

let diffSpeciesComp (sl) =
    sl
    |> List.collect (fun (Step (cl)) -> cl)
    |> List.fold
        (fun cmps cmd ->
            match cmd with
            | Comp (a, b) -> (a, b) :: cmps
            | _ -> cmps)
        []
    |> List.forall (fun (a, b) -> a <> b)

let checkSameSpeciesComp crn =
    if diffSpeciesComp crn.steps then
        None
    else
        Some SameSpeciesComparison

// accepts all commands of program in order. extracted from steps.
let rec condNoFlags cs =
    match cs with
    | Ar (_) :: cs' -> condNoFlags cs'
    | Cond (_) :: _ -> true // if a cmp had preceeded this would not match
    | _ -> false

let checkCondNoFlags crn =
    let allCmds = crn.steps |> List.collect (fun (Step (cl)) -> cl)

    if condNoFlags allCmds then Some CondNoFlags else None

let getFromCnd =
    function
    | IfGE cl -> cl
    | IfGT cl -> cl
    | IfEQ cl -> cl
    | IfLE cl -> cl
    | IfLT cl -> cl

// let writeTwice (Stp cl) =

//     let aux =
//         function
//         | Comp _ -> []
//         | Ar a ->
//             match a with
//             | Ld (_, dst) -> [ dst ]
//             | Add (_, _, dst) -> [ dst ]
//             | Sub (_, _, dst) -> [ dst ]
//             | Mul (_, _, dst) -> [ dst ]
//             | Div (_, _, dst) -> [ dst ]
//             | Sqrt (_, dst) -> [ dst ]
//         | Cond cnd -> cnd |> getFromCnd |> getDst

//     let ls = List.collect aux cl
//     List.distinct ls = ls

(*

let rec writeTwiceCond = function 
    | IfGT(cl) -> cl |> List.collect writeTwiceCommand 
    | IfGE(cl) -> cl |> List.collect writeTwiceCommand  
    | IfEQ(cl) -> cl |> List.collect writeTwiceCommand  
    | IfLT(cl) -> cl |> List.collect writeTwiceCommand  
    | IfLE(cl) -> cl |> List.collect writeTwiceCommand  

and writeTwiceCommand cmd = 
    match cmd with 
    | Ar(a) -> writeTwiceArith a
    | Comp(_) -> [] 
    | Cond(cn) -> writeTwiceCond cn

and writeTwiceCL cl = 
    match cl with 
    | Ar(a)::cl' -> writeTwiceArith a 
    

and writeTwice (stp: Step) = failwith "not implemented" 

let writeTwiceSteps steps = failwith "not implemented"

let writeTwiceConcs concs = failwith "not implemented"

let checkWriteTwice (Crn(cl, sl)) = 
    match writeTwiceConcs cl, writeTwiceSteps sl with
    | None, None -> None  
    | _ -> Some WriteTwice *)

// add the strings isntead of whole thing TODO
let getRWArith a =
    match a with
    | Ld (src, dst) -> Set [ src ], Set [ dst ]
    | Add (src1, src2, dst) -> (Set [ src1; src2 ]), Set [ dst ]
    | Sub (src1, src2, dst) -> (Set [ src1; src2 ]), Set [ dst ]
    | Mul (src1, src2, dst) -> (Set [ src1; src2 ]), Set [ dst ]
    | Div (src1, src2, dst) -> (Set [ src1; src2 ]), Set [ dst ]
    | Sqrt (src, dst) -> Set [ src ], Set[dst]

// Only one constructor but still do this bc......
let getRWComp (c: Comparison) =
    match c with
    | s1, s2 -> Set [ s1; s2 ], Set.empty<Species>

let rec getRWC (cmd: Command) =
    match cmd with
    | Ar (a) -> getRWArith a
    | Comp (c) -> getRWComp c
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

let checkReadWrite crn =
    match
        crn.steps
        |> getRWRL
        |> List.forall (fun (rSet, wSet) -> (rSet, wSet) |> isDisjoint)
    with
    | true -> None
    | _ -> Some CycleConflict

// let checkAll (checks: (CRN -> CRN_Error option) list) (crn: CRN) : CRN_Result =
//     let es =
//         checks
//         |> List.map (fun f -> f crn)
//         |> List.filter (fun e -> e.IsSome)
//         |> List.map (fun e -> e.Value)

//     match es with
//     | [] -> Ok()
//     | es -> Error es

// let analysisTpChkr (input: string) =
//     match parseString input with
//     | Success (ast, _, _) -> checkAll [ checkReadWrite; checkCondNoFlags; checkSameSpeciesComp ] ast
//     | Failure (errorMsg, _, _) -> failwith ("Parsing failed: " + errorMsg)
