module Interpreter

open FParsec
open Types
open Parser
open System

module List =
    let rec foldMap f acc =
        function
        | [] -> [ acc ]
        | head :: tail -> acc :: foldMap f (f acc head) tail

module Option =
    let apply f a b =
        match a, b with
        | Some a, Some b ->
            try
                Some(f a b)
            with
            | :? OverflowException -> None
            | :? DivideByZeroException -> None
        | _ -> None

// Lots of choices regarding the flags. Explain!. ugly.
let comparison (Cmp (Sp (a), Sp (b))) env =

    match Map.tryFind a env, Map.tryFind b env with
    | None, _ -> None
    | _, None -> None
    | Some a, Some b when abs (a - b) < 0.000001 ->
        Some
            { Xgty = true
              Xlty = false
              Ygtx = true
              Yltx = false }
    | Some a, Some b when a > b ->
        Some
            { Xgty = true
              Xlty = false
              Ygtx = false
              Yltx = true }
    | Some a, Some b ->
        Some
            { Xgty = false
              Xlty = true
              Ygtx = true
              Yltx = false }

// Lots of choices regarding the flags. Explain!. ugly.

let updateConcs dst env (newVal: float option) =

    newVal |> Option.map (fun v -> Map.add dst v env)

let applyIfDef op src1 src2 env =
    if env |> Map.containsKey src1 && env |> Map.containsKey src2 then
        try
            Some(op env[src1] env[src2])
        with
        | :? OverflowException -> None
        | :? DivideByZeroException -> None
    else
        None

let applyUnaryIfDef op src env =
    if env |> Map.containsKey src then
        try
            Some(op env[src])
        with
        | :? OverflowException -> None
        | :? DivideByZeroException -> None
    else
        None

let loadIfDef src dst env =
    if env |> Map.containsKey src then
        env |> Map.add dst env[src] |> Some
    else
        None

// Either change to option type or do nothing and rely on type checker. Check the src dst thing only in type checker?
let arithmetic expr concs : Concentrations option =
    match expr with
    | Ld (Sp (a), Sp (b)) -> concs |> applyUnaryIfDef (id) a |> updateConcs b concs
    | Add (Sp (a), Sp (b), Sp (c)) -> concs |> applyIfDef (+) a b |> updateConcs c concs
    | Sub (Sp (a), Sp (b), Sp (c)) -> concs |> applyIfDef (-) a b |> updateConcs c concs
    | Mul (Sp (a), Sp (b), Sp (c)) -> concs |> applyIfDef (*) a b |> updateConcs c concs
    | Div (Sp (a), Sp (b), Sp (c)) -> concs |> applyIfDef (/) a b |> updateConcs c concs
    | Sqrt (Sp (a), Sp (b)) -> concs |> applyUnaryIfDef (sqrt) a |> updateConcs b concs

let updateState (oldState: State) (env: Concentrations option) (flags: Flags option) =
    if env.IsSome && flags.IsSome then
        { status = oldState.status
          concentrations = env.Value
          flags = flags.Value }
    else
        { status = Error
          concentrations = env.Value
          flags = flags.Value }

let rec interpretCmd (cmd: Command) (state: State) =
    match cmd with
    | Ar (a) -> (arithmetic a state.concentrations, Some(state.flags)) ||> updateState state
    | Comp (c) ->
        (Some(state.concentrations), comparison c state.concentrations)
        ||> updateState state
    | Cond (con) -> interpretConditional con state

and interpretCmdList (cmds: CommandList) (state: State) =
    cmds |> List.fold (fun s cmd -> interpretCmd cmd s) state

and interpretConditional con state =
    let flags = state.flags

    match con with
    | IfGT (cmds) when flags.Xgty && flags.Yltx -> interpretCmdList cmds state
    | IfGE (cmds) when flags.Xgty -> interpretCmdList cmds state
    | IfEQ (cmds) when flags.Xgty && flags.Ygtx -> interpretCmdList cmds state
    | IfLT (cmds) when flags.Xlty && flags.Ygtx -> interpretCmdList cmds state
    | IfLE (cmds) when flags.Ygtx -> interpretCmdList cmds state
    | _ -> state

let interpretSteps (steps: StepList) (state: State) =
    steps |> List.fold (fun s (Stp (cl)) -> interpretCmdList cl s) state
(*  match steps with 
    | [] -> state 
    | (Stp(c))::steps -> interpretSteps steps (interpretCmdList c state)  *)

let rec stateSequence steps state =
    seq {
        let s = interpretSteps steps state
        yield s
        yield! stateSequence steps s
    }


let initConcs (concs: ConcList) =
    concs |> List.fold (fun env (Cnc ((Sp s), n)) -> env |> Map.add s n) Map.empty


(*     let rec loop c =
        function
        | [] -> c Map.empty
        | Cnc((Sp s), n) :: cncs -> loop (fun res -> c (res |> Map.add s n)) cncs  
    loop id concs
 *)
let interpret (Crn (concs, steps)) (nSteps: int) =

    let initCncs = initConcs concs

    let state0 =
        { status = Running
          concentrations = initCncs
          flags =
            { Xgty = false
              Xlty = false
              Ygtx = false
              Yltx = false } // initial value of flags. should not matter if well formed program
        }

    (stateSequence steps state0) |> Seq.take nSteps |> Seq.append (seq { state0 })

let analysisIntprt (src: string) (nSteps: int) =
    match parseString src with
    | Success (ast, _, _) -> interpret ast nSteps
    | Failure (errorMsg, _, _) -> failwith ("Parsing failed: " + errorMsg)
