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

type Concentrations = Map<string, float>

type Flags = 
        { Xgty: bool
          Xlty: bool
          Ygtx: bool
          Yltx: bool } 

type State =
    { status: Status
      concentrations: Concentrations
      flags: Flags
    } 

// Lots of choices regarding the flags. Explain! 
let comparison (Cmp(Sp(a), Sp(b))) env =
    if env |> Map.containsKey a && env |> Map.containsKey b then
        let t = 
            match abs(env[a] - env[b]) < 0.000001 with // bc floats... 
            | true -> { Xgty = true; Xlty = false; Ygtx = true; Yltx = false } 
            | _ -> 
                match env[a] > env[b] with
                | true -> { Xgty = true; Xlty = false; Ygtx = false; Yltx = true } 
                | _ ->  { Xgty = false; Xlty = true; Ygtx = true; Yltx = false }
        t |> Some 
    else None
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

// Either change to option type or do nothing and rely on type checker. Check the src dst thing only in type checker?  
let arithmetic expr concs : Map<string, float> option = 
    match expr with 
    | Ld(Sp(a), Sp(b)) -> concs |> applyUnaryIfDef ( id ) a |> updateConcs b concs 
    | Add(Sp(a), Sp(b), Sp(c)) -> concs |> applyIfDef ( + ) a b |> updateConcs c concs 
    | Sub(Sp(a), Sp(b), Sp(c)) -> concs |> applyIfDef ( - ) a b |> updateConcs c concs 
    | Mul(Sp(a), Sp(b), Sp(c)) -> concs |> applyIfDef ( * ) a b |> updateConcs c concs 
    | Div(Sp(a), Sp(b), Sp(c)) -> concs |> applyIfDef ( / ) a b |> updateConcs c concs 
    | Sqrt(Sp(a), Sp(b)) -> concs |> applyUnaryIfDef (sqrt) a |> updateConcs b concs 

let updateState (oldState: State) (env: Map<string, float> option) (flags: Flags option) = 
    if env.IsSome && flags.IsSome then 
        {status = oldState.status; concentrations = env.Value; flags = flags.Value}
    else 
        {status = Error; concentrations = env.Value; flags = flags.Value}

let intepretModule m state =
    match m with 
    | Ar(a) -> (arithmetic a state.concentrations, Some(state.flags)) ||> updateState state   
    | Comp(com) -> (Some(state.concentrations), comparison com state.concentrations) ||> updateState state

let rec interpretCmd (cmd: Command) (state: State) = 
    match cmd with 
    | Mdl(m) -> intepretModule m state 
    | Cond(con) -> interpretConditional con state

and interpretCmdList (cmds: CommandList) (state: State) =
    match cmds with 
    | C(c) -> interpretCmd c state 
    | CL(cl1, cl2) -> interpretCmdList cl1 state |> (interpretCmdList cl2)

and interpretConditional con state =
    let flags = state.flags
    match con with 
    | IfGT(cmds) -> if flags.Xgty && flags.Yltx then interpretCmdList cmds state else state
    | IfGE(cmds) ->  if flags.Xgty then interpretCmdList cmds state else state
    | IfEQ(cmds) -> if flags.Xgty && flags.Ygtx then interpretCmdList cmds state else state
    | IfLT(cmds) ->  if flags.Xlty && flags.Ygtx then interpretCmdList cmds state else state
    | IfLE(cmds) ->  if flags.Ygtx then interpretCmdList cmds state else state

let rec interpretSteps (steps: Step list) (state: State) = 
    match steps with 
    | [] -> state 
    | (St(c))::steps -> interpretSteps steps (interpretCmdList c state)

let rec stateSequence steps state n =
    seq {
      match n with
        | 0 ->  yield interpretSteps steps state 
        | n when n > 0 ->
               let newS =  interpretSteps steps state 
               yield newS 
               yield! stateSequence steps newS (n-1)
        | _ -> failwith "negative n" 
    }

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
    
    let state0 =
        { status = Running
          concentrations = initCncs
          flags = { Xgty = false; Xlty = false; Ygtx = false; Yltx = false } // initial value of flags. should not matter if well formed program
        }

    (stateSequence steps state0 nSteps) |> Seq.append (seq { state0 })
    
let analysis (src: string) (nSteps: int) =
    match parseString src with
    | Success(ast, _, _) -> interpret ast nSteps
    | Failure(errorMsg, _, _) -> failwith ("Parsing failed: " + errorMsg)
