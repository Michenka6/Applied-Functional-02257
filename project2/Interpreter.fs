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

let compare (a, b) (env: Molecules) : Flags option =
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
        molecules = env
        flags = flags }


let rec interpretCommand (cmd: Command) (state: State) : State option =
    match cmd with
    | Ar (a) ->
        option {
            let! env = arithmetic state.molecules a
            return updateState state env state.flags
        }
    | Comp (s1, s2) ->
        option {
            let! flags = compare (s1, s2) state.molecules
            return updateState state state.molecules flags
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

let interpret (crn: CRN) (nSteps: int) =
    let state0 =
        { status = Running
          molecules = crn.molecules
          flags =
            { Xgty = false
              Xlty = false
              Ygtx = false
              Yltx = false } // initial value of flags. should not matter if well formed program
        }

    (crn.steps, state0)
    ||> stateSequence
    |> Seq.take nSteps
    |> Seq.append (seq { state0 })

let analysisIntprt (src: string) (nSteps: int) =
    match parseString src with
    | Success (ast, _, _) -> interpret ast nSteps
    | Failure (errorMsg, _, _) -> failwith ("Parsing failed: " + errorMsg)
