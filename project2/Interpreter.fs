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
type Flags = Map<string, bool> // is continuous somehwo but..

type State =
    { status: Status
      concentrations: Concentrations
      flags: Flags
      nSteps: int }

let comparison com state = failwith "not implemented"

let arithmetic a state = 
    failwith "not implemented"

let interpretConditional con state = failwith "not implemented"

let intepretModule m state =
    match m with 
    | Ar(a) -> arithmetic a state 
    | Comp(com) -> comparison com state 

let interpretCommand (cmd: Command) (state: State) =
    let cncs = state.concentrations 
    match cmd with 
    | Mdl(m) -> intepretModule m state 
    | Cond(con) -> interpretConditional con state

let rec interpretStep (cmds: CommandList) (state: State) =
    match cmds with 
    | C(c) -> interpretCommand c state 
    | CL(cl1, cl2) -> interpretStep cl1 state |> (interpretStep cl2)

let rec interpretSteps (steps: Step list) (state: State) = 
    match steps with 
    | [] -> state 
    | (St(c))::steps -> interpretSteps steps (interpretStep c state)

let rec stateSequence steps state n =
    seq {
      match n with
        | 0 -> yield interpretSteps steps state
        | n when n > 0 -> 
               yield state 
               yield! stateSequence steps state (n-1)
        | _ -> failwith "negative number"
    }

let initFlags = 
    Map.empty |> Map.add "Xgty" false |> Map.add "Xlty" false |> Map.add "Ygtx" false |> Map.add "Xlty" false

let initConcs (concs: Conc list) =
    let rec loop c =
        function
        | [] -> c Map.empty
        | Cn((Sp s), n) :: cncs -> loop (fun res -> c (res |> Map.add s n)) cncs 

    loop id concs

let splitRoot r =
    match r with
    | Cnc(c) -> [c], []
    | Stp(s) -> [], [s]
    //| Conc(s, n) -> [ Conc(s, n) ], []
    //| Step(cmds) -> [], [ Step(cmds) ]

let rec splitRoots (rs: RootList) : Conc list * Step list =
    match rs with
    | R(r) -> splitRoot r
    | RL(rs1, rs2) -> (fun (cs1, stps1) (cs2, stps2) -> cs1 @ cs2, stps1 @ stps2) (splitRoots rs1) (splitRoots rs2)

let interpret (Crn(rs)) (nSteps: int) =
    let concs, steps = splitRoots rs
    let initCncs = initConcs concs
    let initFlgs = initFlags 

    let state0 =
        { status = Running
          concentrations = initCncs
          flags = Map.empty<string, bool>
          nSteps = nSteps }

    (stateSequence steps state0 nSteps) |> Seq.append (seq { state0 })
    
let analysis (src: string) (nSteps: int) =
    match parseString src with
    | Success(ast, _, _) -> interpret ast nSteps
    | Failure(errorMsg, _, _) -> failwith ("Parsing failed: " + errorMsg)
