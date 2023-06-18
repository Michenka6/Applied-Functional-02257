module RxnSim

open FParsec
open Types 
open RxnsParser


let addNewConcs oldState concs = 
    {status = oldState.status; concentrations = concs; flags = oldState.flags}

let countOccurences (s: string) (sl : Species list) = 
    sl |> List.countBy id |> Map.ofList |> Map.tryFind (Sp(s)) |> (fun k -> if k.IsSome then k.Value else 0)

let netChange (s: string) (Rxn(e1, e2, k)) = 
    match e1, e2 with 
    | Empty, Empty -> 0
    | Empty, EL(sl) -> countOccurences s sl 
    | EL(sl), Empty -> -countOccurences s sl 
    | EL(sl1), EL(sl2) -> countOccurences s sl2 - countOccurences s sl1 
    //let nRhs = e2 |> List.countBy id |> Map.ofList |> Map.tryFind s |> (fun k -> if k.IsSome then k.Value else 0)
    //nLhs - nRhs

let prodReactants (Rxn(e1, e2, k)) (state: State) = 
    match e1 with 
    | Empty -> 1.0
    | EL(l) -> 
        l 
        |> List.countBy id 
        |> List.fold (fun prod (Sp(s), m) -> prod * state.concentrations[s] ** m) 1.0

let concODETerm (s: string) (state: State) (Rxn(e1, e2, k) as rxn) = 
    //k * float (netChange s rxn) * (prodReactants rxn state) |> printfn "%A"
    k * float (netChange s rxn) * (prodReactants rxn state)

let simulateTimeStep (delta: float) (state: State) (rxns: Rxns list) (species: string) =
    //rxns |> printfn "%A"
    //rxns |> List.map (netChange "C") |> printfn "%A"
    //rxns |> List.map (concODETerm species state) |> printfn "%A"
    //rxns |> List.map (fun r -> prodReactants r state)  |> printfn "%A"
    
    rxns 
    |> List.map (concODETerm species state)
    |> List.sum
    |> (fun cNext -> state.concentrations[species] + delta * cNext) 
    
let simulateRxnS (delta: float) (rxns: Rxns list) (state: State): State = 
(*     state.concentrations
    |> Map.map (fun s _  -> (simulateTimeStep delta state rxns s)) 
    |> addNewConcs state |> printfn "%A"
 *)
    state.concentrations
    |> Map.map (fun s _  -> (simulateTimeStep delta state rxns s)) 
    |> addNewConcs state  

let extractSpecies (e: Expr) = 
    match e with 
    | Empty -> []
    | EL(l) -> l |> List.map (fun (Sp(s)) -> s) 

let extractAndExtend (state: State) (rxns: Rxns list) = 
    rxns 
    |> List.collect (fun (Rxn(e1, e2, k)) -> (extractSpecies e1) @ (extractSpecies e2))
    |> List.fold (fun (concs: Concentrations) s -> if (concs |> Map.containsKey s) then concs else concs |> Map.add s 0) state.concentrations 
    |> addNewConcs state

let rec simulate (delta: float) (rxns: Rxns list) (state: State) : seq<State> =   
    seq {
            let state = simulateRxnS delta rxns state 
            yield state 
            yield! simulate delta rxns state
    }

let runSim delta s state0 = 
    match parseRxn s with 
    | Success (rxns, _, _) -> simulate delta rxns (rxns |> extractAndExtend state0)
    | Failure (errorMsg, _, _) -> failwith ("Parsing failed: " + errorMsg)




