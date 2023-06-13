module TypeChecker

open FParsec
open AST
open Parser 

let isDisjoint (s1: Set<'a>, s2: Set<'a>) = (s1, s2) ||> Set.intersect = Set.empty

let getRWArith a = 
    match a with 
    | Ld(src, dst) -> Set.empty |> Set.add src, Set.empty |> Set.add dst
    | Add(src1, src2, dst) -> (Set.empty |> Set.add src1 |> Set.add src2), Set.empty |> Set.add dst
    | Sub(src1, src2, dst) -> (Set.empty |> Set.add src1 |> Set.add src2), Set.empty |> Set.add dst 
    | Mul(src1, src2, dst) -> (Set.empty |> Set.add src1 |> Set.add src2), Set.empty |> Set.add dst 
    | Div(src1, src2, dst) -> (Set.empty |> Set.add src1 |> Set.add src2), Set.empty |> Set.add dst 
    | Sqrt(src, dst) -> Set.empty |> Set.add src, Set.empty |> Set.add dst


let getRWModule m = 
    match m with 
    | Ar(a) -> getRWArith a
    | Comp(_) -> Set.empty<'a>, Set.empty<'a> // what do comp

let getRWCond con = failwith "not implemented"
let getRWC c =
    match c with 
    | Mdl(m) -> getRWModule m
    | Cond(con) -> getRWCond con 

// get Read Write Command List
let rec getRWCL cl = 
    match cl with 
    | C(c) -> getRWC c
    | CL(cl1, cl2) -> 
        let (r1, w1) = getRWCL cl1 
        let (r2, w2) = getRWCL cl2 
        (Set.union r1 r2, Set.union w1 w2)

let rec checkRoot r =
    match r with 
    | Conc(_, _) -> true 
    | Step(cl) -> getRWCL cl |> isDisjoint 

let rec checkRs rs : bool =  //(Set<'a> * Set<'a>) list = failwith "lala"
    match rs with 
    | R(r) -> checkRoot r
    | RL(r1, r2) -> checkRs r1 && checkRs r2
    
let checkReadWrite (Crn(rs)) =
    rs |> checkRs 

let analysis (input: string) = 
    match parseString input with 
    | Success (result, _, _) -> printfn $"crn : {result}"
    | Failure (errorMsg, _, _) -> printfn "Parsing failed: %s" errorMsg