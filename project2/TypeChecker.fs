module TypeChecker

open FParsec
//open AST
open Types
open Parser 

// Possibly also check all sources defined here then do no such checks in intepreter. 
type Error = CycleConflict | WriteTwice | SameSpeciesComp | CondNoFlags | SrcOpNotDef 
type Result = NoErrors | Errors of Error list 

let isDisjoint (s1: Set<'a>, s2: Set<'a>) = (s1, s2) ||> Set.intersect = Set.empty


let initConcs (concs: ConcList) =
    concs |> List.fold (fun env (Cnc((Sp s), _)) -> env |> Set.add s) Set.empty


(* let rec srcOpNotDef env0 cl =
    match cl with 


let checkSrcOpNotDef (Crn(cl, sl)) = 
    let env0 = initConcs cl 

 *)

let diffSpeciesComp (sl) =
    sl 
    |> List.collect (fun (Stp(cl)) -> cl)
    |> List.fold 
        (fun cmps cmd -> 
            match cmd with 
            | Comp(Cmp(a, b)) -> (a,b)::cmps
            | _ -> cmps     
        ) List.empty<Species*Species> 
    |> List.forall (fun (a,b) -> a <> b)

let checkSameSpeciesComp(Crn(_, sl)) = 
    match diffSpeciesComp sl with 
    | true -> None
    | _ -> Some SameSpeciesComp

// accepts all commands of program in order. extracted from steps. 
let rec condNoFlags cs =
    match cs with 
    | Ar(_)::cs' -> condNoFlags cs' 
    | Cond(_)::_ -> true // if a cmp had preceeded this would not match 
    | _ -> false  

let checkCondNoFlags(Crn(_, sl)) = 
    let allCmds = sl |> List.collect (fun (Stp(cl)) -> cl) 
    match condNoFlags allCmds with 
    | true -> Some CondNoFlags
    | _ -> None

(* let writeTwiceArith a = 
    match a with 
    | Ld(_, dst) -> dst
    | Add(_, _, dst) -> dst  
    | Sub(_, _, dst) -> dst
    | Mul(_, _, dst) -> dst
    | Div(_, _, dst) -> dst  
    | Sqrt(_, dst) -> dst 

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

let getRWStep (Stp(cl)) = getRWCL cl 

// Get read write set for a root list
let getRWRL (sl: StepList) = 
    sl |> List.map (fun (Stp(cl)) -> getRWCL cl) 

let checkReadWrite (Crn(_, sl)) = 
    match sl |> getRWRL |> List.forall (fun (rSet, wSet) -> (rSet, wSet) |> isDisjoint) with
    | true -> None 
    | _ -> Some CycleConflict 
let checkAll (checks: (CRN -> Error option) list) (crn: CRN) : Result = 
    let es = checks |> List.map (fun f -> f crn) |> List.filter (fun e -> e.IsSome) |> List.map (fun e -> e.Value)
    match es with 
    | [] -> NoErrors
    | es -> Errors(es)

let analysisTpChkr (input: string) = 
    match parseString input with 
    | Success (ast, _, _) -> checkAll [checkReadWrite; checkCondNoFlags; checkSameSpeciesComp] ast
    | Failure (errorMsg, _, _) -> failwith ("Parsing failed: " + errorMsg)