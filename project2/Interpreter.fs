module Interpreter

(*
    For now we assume that Conc statements may not follow Step statements.
    So that program consits of two parts: first Concs, then Steps. 

*)

open FParsec
open AST
open Parser

type Status =
    | Running
    | Error
    | Converged

// Not sure what to name this type
(* type Concentrations =
    { intVariables: Map<string, int>
      floatVariables: Map<string, float> } *)

type Concentrations = Map<string, float>

type State =
    { status: Status
      concentrations: Concentrations
      nSteps: int }

let intepretStep stp state =
    failwith "not implemented"

let rec intepretSteps steps (state: State) = 
    match steps with 
    | [] -> state 
    | stp::steps -> intepretSteps steps (intepretStep stp state)

let rec stateSequence steps state n =
    seq {
      match n with
        | 0 -> yield intepretSteps steps state
        | n when n > 0 -> 
               yield state 
               yield! stateSequence steps state (n-1)
        | _ -> failwith "negative number"
    }

let initializeConcs (concs: Root list) =
    let rec loop c =
        function
        | [] -> c Map.empty
        | Conc((Sp s), n) :: cncs -> loop (fun res -> c (res |> Map.add s n)) cncs
        | _ -> failwith "only expected concs"

    loop id concs

let splitRoot r =
    match r with
    | Conc(s, n) -> [ Conc(s, n) ], []
    | Step(cmds) -> [], [ Step(cmds) ]

let rec splitRoots (rs: RootList) : Root list * Root list =
    match rs with
    | R(r) -> splitRoot r
    | RL(rs1, rs2) -> (fun (cs1, stps1) (cs2, stps2) -> cs1 @ cs2, stps1 @ stps2) (splitRoots rs1) (splitRoots rs2)

let interpret (Crn(rs)) (nSteps: int) =
    let concs, steps = splitRoots rs
    let initCncs = initializeConcs concs

    let state0 =
        { status = Running
          concentrations = initCncs
          nSteps = nSteps }

    (stateSequence steps state0 nSteps) |> Seq.append (seq { state0 })
    
let analysis (src: string) (nSteps: int) =
    match parseString src with
    | Success(ast, _, _) -> interpret ast nSteps
    | Failure(errorMsg, _, _) -> failwith ("Parsing failed: " + errorMsg)
