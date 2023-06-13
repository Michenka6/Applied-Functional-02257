module TypeChecker

open FParsec
open AST
open Parser 


let isDisjoint (s1: Set<'a>, s2: Set<'a>) = (s1, s2) ||> Set.intersect = Set.empty


(* let checkC c = 
    match c with 
    | Mdl(m) -> checkModule m
    | Cond(con) -> checkCond con 

 *)


let getRWArith a = failwith "alala"

let getRWModule m = 
    match m with 
    | Ar(a) -> getRWArith a
    | Comp(_) -> Set.empty<'a>, Set.empty<'a>

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

(* 
    let rec aux (Crn(rs)) (srcs: Set<string>) (dsts: Set<String>) : bool =
        match rs with  
        | Root(r) -> checkRoot(r)
        | RootList(r1, r2) ->  checkRoot(r1) && c
 *)



let analysis (input: string) = 
    match parseString input with 
    | Success (result, _, _) -> printfn $"crn : {result}"
    | Failure (errorMsg, _, _) -> printfn "Parsing failed: %s" errorMsg