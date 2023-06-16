module RxnSim

open FParsec
open Types 
open RxnsParser


let simulateRxn (step: float) (state: State) (rxn: Rxns) : State = failwith "not implemented" 

let simulateRxnS (step: float) (rxns: Rxns list) (state: State): State =
    rxns |> List.fold (simulateRxn step) (state) 

let simulate (T: int) (step: float) (rxns: Rxns list) =
    
    let state0 =
        {   status = Running
            concentrations = Map.empty
            flags = { Xgty = false; Xlty = false; Ygtx = false; Yltx = false } // initial value of flags. should not matter if well formed program
        } 
    
    seq {
        for i in 0..(T-1) do 
            let state = simulateRxnS (float i * step) rxns
            yield state 
    }

let runSim T step s = 
    match parseRxn s with 
    | Success (rxns, _, _) -> simulate T step rxns 
    | Failure (errorMsg, _, _) -> failwith ("Parsing failed: " + errorMsg)
 