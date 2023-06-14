module Interpreter

(*
    For now we assume that Conc statements may not follow Step statements.
    So that program consits of two parts: first Concs, then Steps. 

*)

open FParsec
open AST
open Parser
open System 

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

let interpretConditional con state = failwith "not implemented"


let updateConcs dst env (newVal: float option) = 
    if newVal.IsSome then env |> Map.add dst newVal.Value |> Some else None

let applyIfDef op src1 src2 env =
    if env |> Map.containsKey src1 && env |> Map.containsKey src2 then 
        try 
            Some (op env[src1] env[src2])
        with 
            | :? OverflowException -> None 
            | :? DivideByZeroException -> None
    else None

let applyUnaryIfDef op src env = 
    if env |> Map.containsKey src then
        try 
            Some (op env[src])
        with 
            | :? OverflowException -> None 
            | :? DivideByZeroException -> None
    else None

let loadIfDef src dst env = 
    if env |> Map.containsKey src then env |> Map.add dst env[src] |> Some
    else None

// Either change to option type or do nothing and rely on type checker 
let arithmetic expr concs : Map<string, float> option = 
    match expr with 
    | Ld(Sp(a), Sp(b)) -> concs |> applyUnaryIfDef ( id ) a |> updateConcs b concs 
    | Add(Sp(a), Sp(b), Sp(c)) -> concs |> applyIfDef ( + ) a b |> updateConcs c concs 
    | Sub(Sp(a), Sp(b), Sp(c)) -> concs |> applyIfDef ( - ) a b |> updateConcs c concs 
    | Mul(Sp(a), Sp(b), Sp(c)) -> concs |> applyIfDef ( * ) a b |> updateConcs c concs 
    | Div(Sp(a), Sp(b), Sp(c)) -> concs |> applyIfDef ( / ) a b |> updateConcs c concs 
    | Sqrt(Sp(a), Sp(b)) -> concs |> applyUnaryIfDef (sqrt) a |> updateConcs b concs 

let intepretModule m state =
    match m with 
    | Ar(a) -> 
        let z = arithmetic a state.concentrations
        if z.IsSome then 
            {status = Running; concentrations = z.Value; flags = state.flags; nSteps = state.nSteps}
        else 
            {status = Error; concentrations = state.concentrations; flags = state.flags; nSteps = state.nSteps}
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
