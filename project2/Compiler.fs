module Compiler

open Types
open FParsec
open Parser
open System.Text.RegularExpressions

let matchClkSpecies input =
    let regex = new Regex("X[0-9]+")
    let matches = regex.Matches(input)
    [ for matchObj in matches -> matchObj.Value ]
    |> Set.ofList
    |> List.ofSeq

let oscillatorCrn X1 X2 X3 k = 
    $"rxn[{X1} + {X2}, {X2}+{X2}, {k}]"
    ::$"rxn[{X2} + {X3}, {X3}+{X3}, {k}]"
    ::[$"rxn[{X3} + {X1}, {X1}+{X1}, {k}]"]

let genClkSpecies i k = 
    let X1 = ("X" + string i) 
    let X2 = ("X" + string (i+1)) 
    let X3 = ("X" + string (i+2))  
    oscillatorCrn X1 X2 X3 k, X3, i+3

let getOptionFlags (f1,f2: string option) = 
    match f1, f1 with  
    | Some f1', Some f2' -> f1' + "+", f2' + "+" 
    | Some f1', None  -> f1' + "+", "" 
    | None, Some f2' -> "", f2' + "+" 
    | _, _ -> "", "" 

let ld s1 s2 X3 flags = 
    let f1, f2 = getOptionFlags flags 
    
    $"rxn[{f1} {f2} {s1} + {X3}, {f1} {f2} {s1} + {s2} + {X3}, 1.0]"
    ::[$"rxn[{f1} {f2} {s2} + {X3}, {f2} {f2} e + {X3}, 1.0]"]

let add s1 s2 s3 X3 flags =
    let f1, f2 = getOptionFlags flags 
    
    $"rxn[{f1} {f2} {s1} + {X3}, {f1} {f2} {s1} + {s3} + {X3}, 1.0]"
    ::$"rxn[{f1} {f2} {s2} + {X3}, {f1} {f2} {s2} + {s3} + {X3}, 1.0]"
    ::[$"rxn[{f1} {f2} {s3} + {X3}, e + {X3}, 1.0]"]

let sub s1 s2 s3 X3 flags = 
    let f1, f2 = getOptionFlags flags
 
    $"rxn[{s1} + {X3}, {s1} + {s3} + {X3}, 1.0]"
    ::$"rxn[{f1} {f2} {s2} + {X3}, {f1} {f2} {s2} + H + {X3}, 1.0]"
    ::$"rxn[{f1} {f2} {s3} + {X3}, {f1} {f2} e + {X3}, 1.0]"
    ::[$"rxn[{f1} {f2} {s3} + H + {X3}, {f1} {f2} e + {X3}, 1.0]"]

let mul s1 s2 s3 X3 flags =  
    let f1, f2 = getOptionFlags flags
    
    $"rxn[{f1} {f2} {s1} + {s2} + {X3}, {f1} {f2} {s1} + {s2} + {s3} + {X3}, 1.0]"
    ::[$"rxn[{f1} {f2} {s3} + {X3}, {f1} {f2} e + {X3}, 1.0]"]

let div s1 s2 s3 X3 flags = 
    let f1, f2 = getOptionFlags flags
    
    $"rxn[{f1} {f2} {s1} + {X3}, {f1} {f2} {s1} + {s3} + {X3}, 1.0]"
    ::[$"rxn[{f1} {f2} {s2} + {s3} + {X3}, {f1} {f2} {s2} + {X3}, 1.0]"]

let sqrt s1 s2 X3 flags = 
    let f1, f2 = getOptionFlags flags

    $"rxn[{f1} {f2} {s1} + {X3}, {f1} {f2} {s1} + {s2} + {X3}, 1.0]"
    ::[$"rxn[{f1} {f2} {s2} + {s2} + {X3}, {f1} {f2} e + {X3}, 0.5]"]

let AM X3 (f1, f2) = 
    let f1', f2' = getOptionFlags (f1, f2)
    let B = "B"
    $"rxn[{f1'} {f2'} {X3}, {f2'} {B} + {X3}, 1.0]"
    ::$"rxn[{B} + {f2'} {X3}, {f2'} {f2'} {X3}, 1.0]"
    ::$"rxn[{f2'} {f1'} {X3}, {f1'} {B} + {X3}, 1.0]"
    ::[$"rxn[{B} + {f1'} {X3}, {f1'} {f1'} {X3}, 1.0]"]

let normalize A B X3 (f1, f2) =
    let f1', f2' = getOptionFlags (f1, f2) // should NOT be none here but do this anyway
    
    $"rxn[{f1'} {B} + {X3}, {f2'} {B} + {X3}, 1.0]" 
    ::$"rxn[{f2'} CmpOffset + {X3}, {f1'} CmpOffset + {X3}, 1.0]"
    ::[$"rxn[{f2'} {A} + {X3}, {f1'} {A} + {X3}, 1.0]"] 

let compileCmp (Cmp(A, B)) X3 i =
    let Xgty = "Xgty"
    let Xlty = "Xlty"
    let Ygtx = "Ygtx"
    let Yltx = "Yltx"
    let CmpOffset = "CmpOffset"
    
    let normalxClkRxn, Xnormalx, inext = genClkSpecies i "1.0" 
    let normalyclkRxn, Xnormaly, inext = genClkSpecies inext "1.0"
    let AMxclkRxn, XAMx, inext = genClkSpecies inext "1.0"
    let AMyclkRxn, XAMy, inext = genClkSpecies inext "1.0"

    let normalx = normalize A B Xnormalx (Some Xgty, Some Xlty)
    let normaly = normalize A B Xnormaly (Some Ygtx, Some Yltx)

    let AMx = AM XAMx (Some Xgty, Some Xlty)
    let AMy = AM XAMy (Some Ygtx, Some Yltx)

    normalxClkRxn @ normalx @ normalyclkRxn @ normaly @ AMxclkRxn @ AMx @ AMyclkRxn @ AMy, inext
    
let compileAr a X3 i flags =
    let aux a = 
        match a with
        | Ld(s1, s2) -> ld s1 s2 X3 flags
        | Add(s1, s2, dst) -> add s1 s2 dst X3 flags 
        | Sub(s1, s2, dst) -> sub s1 s2 dst X3 flags
        | Mul(s1, s2, dst) -> mul s1 s2 dst X3 flags
        | Div(s1, s2, dst) -> div s1 s2 dst X3 flags 
        | Sqrt(s1, s2) -> sqrt s1 s2 X3 flags 
    aux a, i

let rec compileCmd cmd X3 i flags =
    match cmd with 
    | Ar a -> compileAr a X3 i flags
    | Comp c -> compileCmp c X3 i // flags reset here, so we do not "old" pass flags
    | Cond c -> compileCond c X3 i


and compileCond c X3 i =
    let Xgty = Some "Xgty"
    let Xlty = Some "Xlty"
    let Ygtx = Some "Ygtx"
    let Yltx = Some "Yltx" 
    
    match c with
    | IfGT(cl) -> compileCl cl X3 i (Xgty, Yltx) //$"[ {compileClCond cl Xgty Yltx}" 
    | IfGE(cl) -> compileCl cl X3 i (Xgty, None)//$"[ {compileClCond cl Xgty None}" 
    | IfEQ(cl) -> compileCl cl X3 i (Xgty, Ygtx)//$"[ {compileClCond cl Xgty Ygtx}" 
    | IfLT(cl) -> compileCl cl X3 i (Ygtx, Xlty)//$"[ {compileClCond cl Ygtx Xlty}"  
    | IfLE(cl) -> compileCl cl X3 i (Ygtx, None)//$"[ {compileClCond cl Ygtx None}" 

// assume only a single cmp per step
and compileCl (cl: CommandList) (X3: string) (i: int) (flags: string option * string option) = 
    match cl with 
    | [] -> [], i
    | Comp c::rest ->  
        let cmpRxns, inext = compileCmp c X3 (i+3) 
        let rxns, _ = compileCl rest X3 i flags
        cmpRxns @ rxns, inext
    | cmd::[] -> compileCmd cmd X3 i flags
    | cmd::cl' -> let rxns, inext = compileCmd cmd X3 i flags // inext should be equal to i in this case 
                  let rxnsRest, j = compileCl cl' X3 i flags // j should be equal to i in this case 
                  rxns @ rxnsRest, j

let compileStep (Stp(cl)) (i: int) =  
    let oscCrn, X3, _ = genClkSpecies i "1.0" 
    let rxns, inext = compileCl cl X3 i (None, None) 
    oscCrn @ rxns, inext 

let compileSteps stps : string list = 
    let rec aux stps i =
        match stps with 
        | [] -> []
        | stp::[] ->
            let rxns, inext = compileStep stp i
            rxns
        | stp::stps' -> 
            let rxns, inext = compileStep stp i
            rxns @ (aux stps' (inext))
    aux stps 1 

let clkConcs (src: string) =
    src 
    |> matchClkSpecies 
    |> List.sort
    |> List.mapi (fun i X -> (X, float (i+1) * 0.1)) 

let initConcs (concs: ConcList) (src: string) =
    let m = Map [ ("Xgty", 0.5); ("Xlty", 0.5); ("Ygtx", 0.5); ("Yltx", 0.5); ("CmpOffset", 0.5)]
    let concs' = concs |> List.map (fun (Cnc(s, n)) -> (s, n)) 
    let concsClk = clkConcs src
    let allConcs = concs' @ concsClk
    allConcs |> List.fold (fun env (k, v) -> env |> Map.add k v) m

let addSeparators (rxns: string list) =
    let rec aux rxns acc = 
        match rxns with 
        | [] -> acc
        | rxn::[] -> acc + rxn + "\n"
        | rxn::rxns' -> aux rxns' (acc + rxn + ",\n" )
    aux rxns ""

let compileCrn (s: string) : State * string = 
    match parseString s with 
    | Success (Crn(concs, stps), _, _ ) -> 
        let rxns = compileSteps stps |> addSeparators  
        let conc0 = initConcs concs rxns  
        let state0 = {status = Running; concentrations = conc0} 
        (state0, rxns)
    | Failure (errorMsg, _, _) -> failwith $"Parsing failed: {errorMsg}"