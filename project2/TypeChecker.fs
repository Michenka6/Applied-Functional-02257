module TypeChecker

open FParsec
open AST
open Parser 

// Possibly also check all sources defined here then do no such checks in intepreter. 
type Error = CycleConflict | ConcAfterStep | WriteTwice | SameSpeciesComp | CondNoFlags 
type Result = NoErrors | Errors of Error list 


let checkConcsStps (Crn(rl)) =
    let rec aux rl =  
        match rl with 
        | Step(_)::Conc(_,_)::_ -> Some ConcAfterStep
        | _::rs -> aux rs 
        | [] -> None  
    aux rl
let isDisjoint (s1: Set<'a>, s2: Set<'a>) = (s1, s2) ||> Set.intersect = Set.empty

let splitRoots (rs: RootList) =
    let rec loop rs (rs1, rs2) : Root list * Root list =
        match rs with
        | [] -> (rs1, rs2)
        | Conc(s, n)::rest -> loop rest (rs1 @ [Conc(s, n)], rs2) // order does not matter may as well use cons
        | Step(s)::rest -> loop rest (rs1, rs2 @ [Step(s)])
    loop rs ([], [])

// add the strings isntead of whole thing TODO
let getRWArith a = 
    match a with 
    | Ld(src, dst) -> Set.empty |> Set.add src, Set.empty |> Set.add dst
    | Add(src1, src2, dst) -> (Set.empty |> Set.add src1 |> Set.add src2), Set.empty |> Set.add dst
    | Sub(src1, src2, dst) -> (Set.empty |> Set.add src1 |> Set.add src2), Set.empty |> Set.add dst 
    | Mul(src1, src2, dst) -> (Set.empty |> Set.add src1 |> Set.add src2), Set.empty |> Set.add dst 
    | Div(src1, src2, dst) -> (Set.empty |> Set.add src1 |> Set.add src2), Set.empty |> Set.add dst 
    | Sqrt(src, dst) -> Set.empty |> Set.add src, Set.empty |> Set.add dst

// Only one constructor but still do this bc......
let getRWComp (c: Comparison) = 
    match c with 
    | Cmp(s1, s2) -> Set.empty |> Set.add s1 |> Set.add s2, Set.empty<Species>

let rec getRWC(cmd: Command) = 
    match cmd with
    | Ar(a) -> getRWArith a 
    | Comp(c) -> getRWComp c 
    | Cond(c) -> getRWCond(c) 

and getRWCL (cl: CommandList) =
    cl |> List.map getRWC |> List.fold (fun (f1, f2) (s1, s2) -> f1 |> Set.union s1, f2 |> Set.union s2) (Set.empty, Set.empty)

and getRWCond (c: Conditional) = 
    match c with 
    | IfGT(cl) -> getRWCL cl
    | IfGE(cl) -> getRWCL cl
    | IfEQ(cl) -> getRWCL cl
    | IfLT(cl) -> getRWCL cl
    | IfLE(cl) -> getRWCL cl

let getRWStep (stp: Root) = 
    match stp with 
    | Step(cl) -> getRWCL cl
    | _ -> failwith "Expected Step"

// Get read write set for a root list
let getRWRL (rs: RootList) = 
    rs |> List.map getRWStep 

let checkReadWrite (Crn(rl)) =
    let _, rs = splitRoots rl
    match rs |> getRWRL |> List.forall (fun (rSet, wSet) -> (rSet, wSet) |> isDisjoint) with
    | true -> None 
    | _ -> Some CycleConflict 
let checkAll (checks: (CRN -> Error option) list) (crn: CRN) : Result = 
    let es = checks |> List.map (fun f -> f crn) |> List.filter (fun e -> e.IsSome) |> List.map (fun e -> e.Value)
    match es with 
    | [] -> NoErrors
    | es -> Errors(es)

let analysisTpChkr (input: string) = 
    match parseString input with 
    | Success (ast, _, _) -> checkAll [checkReadWrite; checkConcsStps] ast
    | Failure (errorMsg, _, _) -> failwith ("Parsing failed: " + errorMsg)