module Compiler

open Types
open FParsec
open Parser

(*
    Compile step takes a list of commands and generates a list of rxns AS A STRing


*)

let oscillatorCrn X1 X2 X3 k = 
    $"rxn[{X1} + {X2}, {X2}+{X2}, {k}], rxn[{X2} + {X3}, {X3}+{X3}, {k}], rxn[{X3} + {X1}, {X1}+{X1}, {k}]"

let getOptionVs (os: string option list) =
    os |> List.map (fun o -> if o.IsSome then o.Value else "") 

let addPlus (os: string list) =
    os |> List.map (fun s -> if s = "" then "" else "+ " + s)
(* 
let expandExpr extras e =
    let extras' = extras |> List.filter (fun s -> s <> "") 
    match e with 
    | Empty -> extras' |> List.fold (fun e s -> e @ [s]) ["e"] |> EL
    | EL(sl) -> extras |> List.fold (fun e s -> e @ [s]) sl |> EL

let expandRxn (Rxn(e1, e2, k)) (flag1: string option) (flag2: string option) (X3: string option) =
    let os = [flag1; flag2; X3] |> getOptionVs 
    let f1 = os.[0]
    let f2 = os.[1]
    let x3 = os.[3] // :D
    
    let e1' = e1 |> expandExpr [f1; x3]
    let e2' = e2 |> expandExpr [f2; x3]
     
    (Rxn(e1', e2', k))


let compileAr a X3= 
    match a with
    | Ld(s1, s2) -> 
        $"rxn[{s1} + {X3}, {s1} + {s2} + {X3}, 1.0], rxn[{s2} + {X3}, e, 1.0]"
    | Add(s1, s2, dst) -> 
        $"rxn[{s1} + {X3}, {s1} + {dst} + {X3}, 1.0], rxn[{s2} + {X3}, {s2} + {dst} + {X3}, 1.0], rxn[{dst} + {X3}, e + {X3}, 1.0]"
    | Sub(s1, s2, dst) ->
        $"rxn[{s1} + {X3}, {s1} + {dst} + {X3}, 1.0], rxn[{s2} + {X3}, {s2} + H + {X3}, 1.0], rxn[{dst} + {X3}, e + {X3}, 1.0], rxn[{dst} + H + {X3}, e + {X3}, 1.0]"
    | Mul(s1, s2, dst) -> 
        $"rxn[{s1} + {s2} + {X3}, {s1} + {s2} + {dst} + {X3}, 1.0], rxn[{dst} + {X3}, e + {X3}, 1.0]"
    | Div(s1, s2, dst) -> 
        $"rxn[{s1} + {X3}, {s1} + {dst} + {X3}, 1.0], rxn[{s2} + {dst} + {X3}, {s2} + {X3}, 1.0]"
    | Sqrt(s1, s2) ->
        $"rxn[{s1} + {X3}, {s1} + {s2} + {X3}, 1.0], rxn[{s2} + {s2} + {X3}, e + {X3}, 0.5]"

let compileAr' a flag1 string flag2 = 
    
    let getFlags (flag1: string option) (flag2: string option) : string * string = 
        match flag1.IsSome, flag2.IsSome with 
            | true, true -> flag1.Value, flag2.Value
            | true, false -> flag1.Value, ""
            | false, true -> "", flag2.Value
            | _, _ -> "", ""
    let f1, f2 = getFlags flag1 flag2 
    match a with 
    | Ld(s1, s2) -> 
        $"rxn[{f1}+{f2}+{s1}, {f1}+{f2}+{s1}+{s2}, 1.0], rxn[{f1}+{f2}+{s2}, {f1}+{f2}+e, 1.0]"
    | Add(s1, s2, dst) -> 
        $"rxn[{f1}+{f2}+{s1}, {f1}+{f2}+{s1} + {dst}, 1.0], rxn[{f1}+{f2}+{s2}, {f1}+{f2}+{s2} + {dst}, 1.0], rxn[{f1}+{f2}+{dst}, {f1}+{f2}+e, 1.0]"
    | Sub(s1, s2, dst) ->
        $"rxn[{f1}+{f2}+{s1}, {f1}+{f2}+{s1} + {dst}, 1.0], rxn[{f1}+{f2}+{s2}, {f1}+{f2}+{s2} + H, 1.0], rxn[{f1}+{f2}+{dst}, {f1}+{f2}+e, 1.0], rxn[{f1}+{f2}+{dst} + H, {f1}+{f2}+e, 1.0]"
    | Mul(s1, s2, dst) -> 
        $"rxn[{f1}+{f2}+{s1} + {s2}, {f1}+{f2}+{s1} + {s2} + {dst}, 1.0], rxn[{f1}+{f2}+{dst}, {f1}+{f2}+e, 1.0]"
    | Div(s1, s2, dst) -> 
        $"rxn[{f1}+{f2}+{s1}, {f1}+{f2}+{s1} + {dst}, 1.0], rxn[{f1}+{f2}+{s2} + {dst}, {f1}+{f2}+{s2}, 1.0]"
    | Sqrt(s1, s2) ->
        $"rxn[{f1}+{f2}+{s1}, {f1}+{f2}+{s1} + {s2}, 1.0], rxn[{f1}+{f2}+{s2} + {s2}, {f1}+{f2}+e, 0.5]"




let AM gtFlag ltFlag = 
    let B = "B"
    $"[rxn[{gtFlag} + {ltFlag}, {ltFlag} + {B}, 1.0],\n"
    + $"rxn[{B} + ltFlag, {ltFlag} + {ltFlag}, 1.0],\n"
    + $"rxn[{ltFlag} + {gtFlag}, {gtFlag} + {B}, 1.0],\n"
    + $"rxn[{B} + {gtFlag}, {gtFlag} + {gtFlag}, 1.0]]"

let compileCmp (Cmp(A, B:string)) =
    let Xgty = "Xgty"
    let Xlty = "Xlty"
    let Ygtx = "Ygtx"
    let Yltx = "Yltx"
    let CmpOffset = "CmpOffset"


    let normalize = 
        "[rxn[Xgty +" + B + ", Xlty +" + B + ", 1.0],\n"
        + "rxn[Xlty + CmpOffset, Xgty + CmpOffset, 1.0],\n"
        + "rxn[Xlty + " + A + ", Xgty + " + A + ", 1.0],\n" 
        +
        "rxn[Ygtx + " + A + ", Yltx +" + A + ", 1.0],\n"
        + "rxn[Yltx + CmpOffset, Ygtx + CmpOffset, 1.0],\n"
        + "rxn[Yltx +" + B + ", Ygtx +" + B + ", 1.0]]"

    let AMx =  AM Xgty Xlty
    let AMy = AM Ygtx Yltx


    normalize + ";" + AMx + ";" + AMy 
  

let rec compileCond c = 
    let Xgty = Some "Xgty"
    let Xlty = Some "Xlty"
    let Ygtx = Some "Ygtx"
    let Yltx = Some "Yltx"
    match c with
    | IfGT(cl) -> $"[ {compileClCond cl Xgty Yltx}" 
    | IfGE(cl) -> $"[ {compileClCond cl Xgty None}" 
    | IfEQ(cl) -> $"[ {compileClCond cl Xgty Ygtx}" 
    | IfLT(cl) -> $"[ {compileClCond cl Ygtx Xlty}"  
    | IfLE(cl)-> $"[ {compileClCond cl Ygtx None}" 


and compileCl cl X1 X2 X3 = 
    match cl with
    | [] -> ""
    | Ar a :: [] -> $"{compileAr a} ]"
    | Comp c :: [] -> $"{compileCmp c}"
    | Cond c :: [] -> $"{compileCond c}"
    | Ar a :: Comp c :: [] -> $"{compileAr a}]; \n\n{compileCmp c}"
    | Ar a :: Comp c :: cl' -> $"{compileAr a} ]; \n\n{compileCmp c}  \n\n{compileCl (cl')}"
    | Ar a :: cl' -> $"{compileAr a},\n\n{compileCl (cl')}"
    | Comp c :: Comp c' :: cl -> $"{compileCmp c};\n\n{compileCmp (c')};\n\n {compileCl cl}"
    | Comp c :: cl  -> $"{compileCmp c};\n\n[{compileCl cl}"
    //| Cond c :: Ar a :: [] -> $"{compileCond c},\n\n{compileAr (a)}"  
    | Cond c :: cl' -> $"{compileCond c};\n\n{compileCl (cl')}"  
    
and compileClCond cl gtFlag ltFlag = 
    match cl with
    | [] -> ""
    | Ar a :: [] -> $"{compileAr' a} ]"
    | Comp c :: [] -> $"{compileCmp c}"
    | Cond c :: [] -> $"{compileCond c}"
    | Ar a :: Comp c :: [] -> $"{compileAr' a}]; \n\n{compileCmp c}"
    | Ar a :: Comp c :: cl' -> $"{compileAr' a} ]; \n\n{compileCmp c}  \n\n{compileCl (cl')}"
    | Ar a :: cl' -> $"{compileAr' a},\n\n{compileCl (cl')}"
    | Comp c :: Comp c' :: cl -> $"{compileCmp c};\n\n{compileCmp (c')};\n\n {compileCl cl}"
    | Comp c :: cl  -> $"{compileCmp c};\n\n[{compileCl cl}"
    //| Cond c :: Ar a :: [] -> $"{compileCond c},\n\n{compileAr (a)}"  
    | Cond c :: cl' -> $"{compileCond c};\n\n{compileCl (cl')}"  
 
let compileStep (Stp cl) = 
    match cl with 
    | Ar a::cl' -> "[" + $"{compileCl cl}"
    |_ -> $"{compileCl cl}" 

let initConcs (concs: ConcList) =
    let m = Map [ ("Xgty", 0.5); ("Xlty", 0.5); ("Ygtx", 0.5); ("Yltx", 0.5); ("CmpOffset", 0.5)]
    concs |> List.fold (fun env (Cnc (s, n)) -> env |> Map.add s n) m

(* let getNCmps stps =  
    match stps with 
    | 
 *)

let compileCrn (Crn(concs, stps)) =
    let state0 = {status = Running; concentrations = initConcs concs} 
(*     let rec aux = function 
        | [] -> ""
        | stp::[] -> compileStep stp
        | stp::stps -> compileStep stp + ";" + aux stps  *)
    let src = stps |> List.fold (fun s stp -> s + (compileStep stp) + ";") ""
    (state0, src)

let compileCrnPP (s: string) = 
    match parseString s with 
    | Success (result, _, _) -> compileCrn result
    | Failure (errorMsg, _, _) -> failwith $"Parsing failed: {errorMsg}" *)

let compileCl cl i = 
    failwith "Not implemented"

let compileStep (Stp(cl)) (i: int) = 
    let oscCrn = oscillatorCrn ("X" + string i) ("X" + string (i+1)) ("X" + string (i+2)) "1.0"
    let rxns, inext = compileCl cl i 
    oscCrn + rxns, (inext + 3)

let compileSteps stps : string = 
    let rec aux stps i =
        match stps with 
        | [] -> ""
        | stp::[] ->
            let rxns, inext = compileStep stp i
            rxns
        | stp::stps' -> 
            let rxns, inext = compileStep stp i
            rxns + "," + (aux stps' (inext))
            //aux stps' (i+1) (compileStep stp i :: acc) 
    aux stps 1 

(*     let state0 = initConcs concs 
    let rxns =  *)
let initConcs (concs: ConcList) = failwith "not implemented" 

let compileCrn (s: string) : State * string = 
    match parseString s with 
    | Success (Crn(concs, stps), _, _ ) -> 
        let state0 = initConcs concs
        let rxns = compileSteps stps 
        (state0, rxns)
    | Failure (errorMsg, _, _) -> failwith $"Parsing failed: {errorMsg}"