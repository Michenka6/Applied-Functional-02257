module RxnSim

open FParsec
open Types 
open RxnsParser


let simulateRxn rxn = failwith "not implemented" 

let simulateRxnS (step: float) (rxns: Rxns list)  = failwith "not implemented"

let simulate (T: int) (step: float) (rxns: Rxns list) =
    seq {
        for i in 0..(T-1) do 
            yield simulateRxnS (float i * step) rxns
    }

let runSim T step s = 
    match parseRxn s with 
    | Success (rxns, _, _) -> simulate T step rxns 
    | Failure (errorMsg, _, _) -> failwith ("Parsing failed: " + errorMsg)
 