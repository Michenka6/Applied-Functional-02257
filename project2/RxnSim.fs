module RxnSim

open FParsec
open Rxns
open RxnsParser

let simulate rxn = failwith "not implemented"

let runSim s = 
    match parseRxn s with 
    | Success (rxn, _, _) -> simulate rxn
    | Failure (errorMsg, _, _) -> failwith ("Parsing failed: " + errorMsg)
 