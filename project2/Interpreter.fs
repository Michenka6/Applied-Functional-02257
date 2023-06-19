module Interpreter

open FParsec
open Types
open Parser
open System

// Lots of choices regarding the flags. Explain!. ugly.

let setFlags a b c d =
    { Xgty = a
      Xlty = b
      Ygtx = c
      Yltx = d }

let compare ((a, b): Comparison) (env: Molecules) : Flags option =
    option {
        let! a = Map.tryFind a env
        let! b = Map.tryFind b env

        let flags =
            if abs (a - b) < 0.000001 then
                setFlags true false true false
            else if a > b then
                setFlags true false false true
            else
                setFlags false true true false

        return flags
    }

// Lots of choices regarding the flags. Explain!. ugly.

let updateEnv (key: Species) (env: Molecules) (value: Number) =
    option {
        let! _ = Map.tryFind key env
        return Map.add key value env
    }

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
let safeDiv a b =
    match b with
    | 0.0 -> None
    | _ -> Some(a / b)

let safeSqrt a = if a < 0.0 then None else Some(sqrt a)

let arithmetic (env: Molecules) (arithmetic: Arithmetic) : Molecules option =
    match arithmetic with
    | Ld (a, b) ->
        option {
            let! value = Map.tryFind a env
            return Map.add b value env
        }
    | Add (a, b, c) ->
        option {
            let! aValue = Map.tryFind a env
            let! bValue = Map.tryFind b env
            return Map.add c (aValue + bValue) env
        }
    | Sub (a, b, c) ->
        option {
            let! aValue = Map.tryFind a env
            let! bValue = Map.tryFind b env
            return Map.add c (aValue - bValue) env
        }
    | Mul (a, b, c) ->
        option {
            let! aValue = Map.tryFind a env
            let! bValue = Map.tryFind b env
            return Map.add c (aValue * bValue) env
        }
    | Div (a, b, c) ->
        option {
            let! aValue = Map.tryFind a env
            let! bValue = Map.tryFind b env
            let! cValue = safeDiv aValue bValue
            return Map.add c cValue env
        }
    | Sqrt (a, b) ->
        option {
            let! aValue = Map.tryFind a env
            let! bValue = safeSqrt aValue
            return Map.add b bValue env
        }

let updateState (oldState: State) (env: Molecules) (flags: Flags) =
    { oldState with
        concentrations = env
        flags = flags }


let rec interpretCommand (cmd: Command) (state: State) : State option =
    match cmd with
    | Ar (a) ->
        option {
            let! env = arithmetic state.concentrations a
            return updateState state env state.flags
        }
    | Comp (c) ->
        option {
            let! flags = compare c state.concentrations
            return updateState state state.concentrations flags
        }
    | Cond (con) -> Some(interpretConditional con state)

and interpretCmdList (commands: Command list) (state: State) : State =
    (state, commands)
    ||> List.fold (fun state command ->
        match interpretCommand command state with
        | None -> state
        | Some s -> s)

and interpretConditional con state =
    let flags = state.flags

    match con with
    | IfGT (cmds) when flags.Xgty && flags.Yltx -> interpretCmdList cmds state
    | IfGE (cmds) when flags.Xgty -> interpretCmdList cmds state
    | IfEQ (cmds) when flags.Xgty && flags.Ygtx -> interpretCmdList cmds state
    | IfLT (cmds) when flags.Xlty && flags.Ygtx -> interpretCmdList cmds state
    | IfLE (cmds) when flags.Ygtx -> interpretCmdList cmds state
    | _ -> state

let interpretSteps (steps: Step list) (state: State) =
    (state, steps)
    ||> List.fold (fun state (Step commands) -> interpretCmdList commands state)

let rec stateSequence steps state =
    seq {
        let s = interpretSteps steps state
        yield s
        yield! stateSequence steps s
    }

let initConcs (concs: Concentration list) =
    concs |> List.fold (fun env (s, n) -> env |> Map.add s n) Map.empty

let interpret (crn: CRN) (nSteps: int) =

    let initCncs = initConcs crn.concentrations

    let state0 =
        { status = Running
          concentrations = initCncs
          flags =
            { Xgty = false
              Xlty = false
              Ygtx = false
              Yltx = false } // initial value of flags. should not matter if well formed program
        }

    (stateSequence crn.steps state0)
    |> Seq.take nSteps
    |> Seq.append (seq { state0 })

let analysisIntprt (src: string) (nSteps: int) =
    match parseString src with
    | Success (ast, _, _) -> interpret ast nSteps
    | Failure (errorMsg, _, _) -> failwith ("Parsing failed: " + errorMsg)
