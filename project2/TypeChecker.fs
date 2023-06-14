module TypeChecker

open FParsec
open AST
open Parser 

type Error = CycleConflict | ConcAfterStep | WriteTwice | SameSpeciesComp | CondNoFlags
type Result = NoErrors | Errors of Error list 

let isDisjoint (s1: Set<'a>, s2: Set<'a>) = (s1, s2) ||> Set.intersect = Set.empty

(* let getRWArith a = 
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
    | Comp(_) -> failwith "not implemented" //Set.empty<'a>, Set.empty<'a> // what do comp

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
 *)
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
let checkReadWrite (rs: RootList) =  
    match rs |> getRWRL |> List.forall (fun (rSet, wSet) -> (rSet, wSet) |> isDisjoint) with
    | true -> []
    | _ -> [CycleConflict]

let analysisTpChkr (input: string) = 
    match parseString input with 
    | Success (Crn(rs), _, _) ->
        let concs, rs = splitRoots rs 
        let res = checkReadWrite rs 
        match res with 
        | [] -> NoErrors |> printfn "%A"
        | e -> Errors(e) |> printfn "%A"

        res |> printfn "%A"
    | Failure (errorMsg, _, _) -> printfn "Parsing failed: %s" errorMsg