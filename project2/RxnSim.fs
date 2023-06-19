module RxnSim

open FParsec
open Types 
open RxnsParser

let addNewConcs oldState concs = 
    {status = oldState.status; concentrations = concs; flags = oldState.flags}

let countOccurences (s: Species) (sl : Species list) = 
    sl |> List.countBy id |> Map.ofList |> Map.tryFind s |> (fun k -> if k.IsSome then k.Value else 0)

let netChange (s: Species) (Rxn(e1, e2, k)) = 
    match e1, e2 with 
    | Empty, Empty -> 0
    | Empty, EL(sl) -> countOccurences s sl 
    | EL(sl), Empty -> -countOccurences s sl 
    | EL(sl1), EL(sl2) -> countOccurences s sl2 - countOccurences s sl1 

let prodReactants (Rxn(e1, e2, k)) (state: State) = 
    match e1 with 
    | Empty -> 1.0
    | EL(l) -> 
        l 
        |> List.countBy id 
        |> List.fold (fun prod (s, m) -> prod * state.concentrations[s] ** m) 1.0

let concODETerm (s: Species) (state: State) (Rxn(_, _, k) as rxn) = 
    k * float (netChange s rxn) * (prodReactants rxn state)

let slope (state: State) (rxns: Rxns list) (s: Species) =    
    rxns 
    |> List.map (concODETerm s state) 
    |> List.sum

let rungeKutta (f: State -> Rxns list -> Species -> float) h state rxns s = 
    let yn = state.concentrations[s]
    let k1 = f state rxns s
    let k2 = f (addNewConcs state (state.concentrations |> Map.add s (yn + 0.5 * h * k1))) rxns s
    let k3 = f (addNewConcs state (state.concentrations |> Map.add s (yn + 0.5 * h * k2))) rxns s 
    let k4 = f (addNewConcs state (state.concentrations |> Map.add s (yn + h * k3))) rxns s 
    
    yn + h / 6.0 * (k1 + 2.0*k2 + 2.0*k3 + k4)

let simulateTimeStep (delta: float) (state: State) (rxns: Rxns list) (species: Species) =    
    rxns 
    |> List.map (concODETerm species state)
    |> List.sum
    |> (fun dsdt -> state.concentrations[species] + delta * dsdt) 
    
let simulateRxnS (delta: float) (rxns: Rxns list) (state: State): State = 
    state.concentrations
    |> Map.map (fun s _  -> (simulateTimeStep delta state rxns s)) 
    |> addNewConcs state  

let simulateRxnS_ (delta: float) (rxns: Rxns list) (state: State): State = 
    state.concentrations
    |> Map.map (fun s _  -> (rungeKutta slope delta state rxns s)) 
    |> addNewConcs state  

let euler (f: State -> Rxns list -> Species -> float) (delta: float) (state: State) (rxns: Rxns list) (species: Species) =  
    state.concentrations[species] + delta * (f state rxns species)

let simulateRxns (simTimeStep) (f: State-> Rxns list -> Species -> float) (delta: float) (rxns: Rxns list) (state: State): State = 
    state.concentrations
    |> Map.map (fun s _  -> (simTimeStep f delta state rxns s)) 
    |> addNewConcs state  

let extractSpecies (e: Expr) = 
    match e with 
    | Empty -> []
    | EL(l) -> l |> List.map (fun s -> s) 

let extractAndExtend (state: State) (rxns: Rxns list) = 
    rxns 
    |> List.collect (fun (Rxn(e1, e2, k)) -> (extractSpecies e1) @ (extractSpecies e2))
    |> List.fold (fun (concs: Concentrations) s -> if (concs |> Map.containsKey s) then concs else concs |> Map.add s 0) state.concentrations 
    |> addNewConcs state

let rec simulate (delta: float) (rxns: Rxns list) (state: State) : seq<State> =   
    seq {
            let state = simulateRxns rungeKutta slope delta rxns state 
            yield state 
            yield! simulate delta rxns state
    }

let runSim delta s state0 = 
    match parseRxn s with 
    | Success (rxns, _, _) -> simulate delta rxns (rxns |> extractAndExtend state0)
    | Failure (errorMsg, _, _) -> failwith ("Parsing failed: " + errorMsg)




