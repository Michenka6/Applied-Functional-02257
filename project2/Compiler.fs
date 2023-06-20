module Compiler

open Types
open FParsec
open Parser

(*
    Compile step takes a list of commands and generates a list of rxns AS A STRing


*)

let compileAr = function 
    | Ld(s1, s2) -> 
        $"rxn[{s1}, {s1} + {s2}, 1.0], rxn[{s2}, e, 1.0]"
    | Add(s1, s2, dst) -> 
        $"rxn[{s1}, {s1} + {dst}, 1.0], rxn[{s2}, {s2} + {dst}, 1.0], rxn[{dst}, e, 1.0]"
    | Sub(s1, s2, dst) ->
        $"rxn[{s1}, {s1} + {dst}, 1.0], rxn[{s2}, {s2} + H, 1.0], rxn[{dst}, e, 1.0], rxn[{dst} + H, e, 1.0]"
    | Mul(s1, s2, dst) -> 
        $"rxn[{s1} + {s2}, {s1} + {s2} + {dst}, 1.0], rxn[{dst}, e, 1.0]"
    | Div(s1, s2, dst) -> 
        $"rxn[{s1}, {s1} + {dst}, 1.0], rxn[{s2} + {dst}, {s2}, 1.0]"
    | Sqrt(s1, s2) ->
        $"rxn[{s1}, {s1} + {s2}, 1.0], rxn[{s2} + {s2}, e, 0.5]"

let compileCmp (Cmp(A, B)) =
    $"[rxn[Xgt + {B}, Xlt + {B}, 1.0], rxn[Xlt + {A}, Xgt + {A}, 0.5],
rxn[Ygt + {A}, Ylt + {A}, 1.0], rxn[Ylt + {B}, Ygt + {B}, 1.0]];\n\n
[rxn[Xgt + Xlt, Xlt + {B}, 1.0], rxn[{B} + Xlt, Xlt + Xlt, 1.0],
rxn[Xlt + Xgt, Xgt + {B}, 1.0], rxn[{B} + Xgt, Xgt + Xgt, 1.0],
rxn[Ygt + Ylt, Ylt + {A}, 1.0], rxn[{A} + Ylt, Ylt + Ylt, 1.0],
rxn[Ylt + Ygt, Ygt + {A}, 1.0], rxn[{A} + Ygt, Ygt + Ygt, 1.0]]"

let rec compileCond = function  
    | IfGT(cl) -> $"[ {compileCl cl}" 
    | IfGE(cl) -> $"[ {compileCl cl}" 
    | IfEQ(cl) -> $"[ {compileCl cl}" 
    | IfLT(cl) -> $"[ {compileCl cl}"  
    | IfLE(cl)-> $"[ {compileCl cl}" 


and compileCl cl = 
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
    

let compileStep (Stp cl) = 
    match cl with 
    | Ar a::cl' -> "[" + $"{compileCl cl}"
    |_ -> $"{compileCl cl}" 

let initConcs (concs: ConcList) =
    let m = Map [ ("Xgty", 0.0); ("Xlty", 0.0); ("Ygtx", 0.0); ("Yltx", 0.0) ]
    concs |> List.fold (fun env (Cnc (s, n)) -> env |> Map.add s n) m


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
    | Failure (errorMsg, _, _) -> failwith $"Parsing failed: {errorMsg}"
 