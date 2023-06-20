module RxnSim

open FParsec
open Types
open RxnsParser
open System
open Microsoft.FSharp.Core.Operators.Checked

let addNewConcs oldState concs =
    { status = oldState.status
      concentrations = concs }

let extractSpecies (e: Expr) =
    match e with
    | Empty -> []
    | EL (l) -> l |> List.map (fun s -> s)

let extractAndExtend (state: State) (rxns: Rxns list) =
    rxns
    |> List.collect (fun (Rxn (e1, e2, k)) -> (extractSpecies e1) @ (extractSpecies e2))
    |> List.fold
        (fun (concs: Molecules) s ->
            if (concs |> Map.containsKey s) then
                concs
            else
                concs |> Map.add s 0)
        state.concentrations
    |> addNewConcs state

let countOccurences (s: Species) (sl: Species list) =
    sl
    |> List.countBy id
    |> Map.ofList
    |> Map.tryFind s
    |> (fun k -> if k.IsSome then k.Value else 0)

let netChange (s: Species) (Rxn (e1, e2, k)) =
    match e1, e2 with
    | Empty, Empty -> 0
    | Empty, EL (sl) -> countOccurences s sl
    | EL (sl), Empty -> -countOccurences s sl
    | EL (sl1), EL (sl2) -> countOccurences s sl2 - countOccurences s sl1

let prodReactants (Rxn (e1, e2, k)) (state: State) =
    match e1 with
    | Empty -> 1.0
    | EL (l) ->
        l
        |> List.countBy id
        |> List.fold (fun prod (s, m) -> prod * state.concentrations[s] ** m) 1.0

let concODETerm (s: Species) (state: State) (Rxn (_, _, k) as rxn) =
    k * float (netChange s rxn) * (prodReactants rxn state)

let slope (state: State) (rxns: Rxns list) (s: Species) =
    rxns |> List.map (concODETerm s state) |> List.sum

let rungeKutta (f: State -> Rxns list -> Species -> float) h state rxns s =
    let yn = state.concentrations[s]
    let k1 = f state rxns s

    let k2 =
        f (addNewConcs state (state.concentrations |> Map.add s (yn + 0.5 * h * k1))) rxns s

    let k3 =
        f (addNewConcs state (state.concentrations |> Map.add s (yn + 0.5 * h * k2))) rxns s

    let k4 =
        f (addNewConcs state (state.concentrations |> Map.add s (yn + h * k3))) rxns s

    yn + h / 6.0 * (k1 + 2.0 * k2 + 2.0 * k3 + k4)

let simulateTimeStep (delta: float) (state: State) (rxns: Rxns list) (species: Species) =
    rxns
    |> List.map (concODETerm species state)
    |> List.sum
    |> (fun dsdt -> state.concentrations[species] + delta * dsdt)

let simulateRxnS (delta: float) (rxns: Rxns list) (state: State) : State =
    state.concentrations
    |> Map.map (fun s _ -> (simulateTimeStep delta state rxns s))
    |> addNewConcs state

let simulateRxnS_ (delta: float) (rxns: Rxns list) (state: State) : State =
    state.concentrations
    |> Map.map (fun s _ -> (rungeKutta slope delta state rxns s))
    |> addNewConcs state

let euler
    (f: State -> Rxns list -> Species -> float)
    (delta: float)
    (state: State)
    (rxns: Rxns list)
    (species: Species)
    =
    let yn = state.concentrations[species] + delta * (f state rxns species)
    let result = (state.concentrations[species] + yn) / 2.0
    if yn <= 0.0 || yn > 0.05 * result then 0.0 else result

let multistep f delta (coeffs: float list) (states: State list) (rxns: Rxns list) (species: Species) =
    (coeffs, states)
    ||> List.zip
    |> List.fold (fun sum (b, s) -> sum + delta * b * (f s rxns species)) 0.0
    |> (fun x -> (List.head states).concentrations[species] + x)
    |> (fun x -> if x <= 0.0 then 0.0 else x)

let adamsBashforth2 (f: State -> Rxns list -> Species -> float) delta states rxns species =
    multistep f delta [ 1.5; -0.5 ] states rxns species

let trapezoidal
    (f: State -> Rxns list -> Species -> float)
    (delta: float)
    (state: State)
    (rxns: Rxns list)
    (species: Species)
    : float =
    let y = state.concentrations[species]

    let yPredict = y + delta * f state rxns species

    let concPredict = state.concentrations |> Map.add species yPredict

    let statePredict =
        { status = state.status
          concentrations = concPredict }

    let yCorrected =
        y + 0.5 * delta * ((f state rxns species) + (f statePredict rxns species))

    yCorrected


let simulateRxns
    (simTimeStep)
    (f: State -> Rxns list -> Species -> float)
    (delta: float)
    (rxns: Rxns list)
    (state: State)
    : State =
    state.concentrations
    |> Map.map (fun s _ -> (simTimeStep f delta state rxns s))
    |> addNewConcs state


let rec simulate (delta: float) (rxns: Rxns list) (state: State) : seq<State> =
    seq {
        //let state = simulateRxns rungeKutta slope delta rxns state
        yield state
        yield! simulate delta rxns (simulateRxns trapezoidal slope delta rxns state)
    }

let simulateRxnsMulti
    simTimeStep
    (f: State -> Rxns list -> Species -> float)
    (delta: float)
    (rxns: Rxns list)
    (states: State list)
    : State =
    states
    |> List.head
    |> (fun s -> s.concentrations)
    |> Map.map (fun s _ -> (simTimeStep f delta states rxns s))
    |> addNewConcs (List.head states)

let rec simulateMulti delta (rxns: Rxns list) (states: State list) : seq<State> =
    seq {
        yield List.head states
        let newState = simulateRxnsMulti adamsBashforth2 slope delta rxns states
        let newStates = newState :: [ List.head states ]
        yield! simulateMulti delta rxns newStates
    }
    |> Seq.append (seq { List.last states })


let setupAB f delta rxns state =
    simulateRxns euler f delta rxns state :: [ state ]

let sim delta rxns state0 =
    //let state = rxns |> extractAndExtend state0
    //simulateMulti delta rxns (setupAB slope delta rxns state)
    simulate delta rxns (rxns |> extractAndExtend state0)

let runSim delta s state0 =
    match parseRxn s with
    | Success (rxns, _, _) -> sim delta rxns state0
    | Failure (errorMsg, _, _) -> failwith ("Parsing failed: " + errorMsg)
