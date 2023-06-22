module Interpreter

open FParsec
open Types
open Parser
open System

// Lots of choices regarding the flags. Explain!. ugly.

// let setFlags a b c d =
//     { Xgty = a
//       Xlty = b
//       Ygtx = c
//       Yltx = d }

// let compare (a, b) (env: State) : Flags option =
//     option {
//         let! a = Map.tryFind a env
//         let! b = Map.tryFind b env

//         let flags =
//             if abs (a - b) < 0.000001 then
//                 setFlags true false true false
//             else if a > b then
//                 setFlags true false false true
//             else
//                 setFlags false true true false

//         return flags
// }

let initConcs (concs: ConcList) =
    let m = Map [ ("Xgty", 0.0); ("Xlty", 0.0); ("Ygtx", 0.0); ("Yltx", 0.0) ]
    concs |> List.fold (fun env (Cnc ((s), n)) -> env |> Map.add s n) m

let compare (a, b) env =

    match Map.tryFind a env, Map.tryFind b env with
    | None, _ -> None
    | _, None -> None
    | Some a, Some b when abs (a - b) < 0.000001 -> Some [ "Xgty", 1.0; "Xlty", 0.0; "Ygtx", 1.0; "Yltx", 0.0 ]
    | Some a, Some b when a > b -> Some [ "Xgty", 1.0; "Xlty", 0.0; "Ygtx", 0.0; "Yltx", 1.0 ]
    | Some a, Some b -> Some [ "Xgty", 0.0; "Xlty", 1.0; "Ygtx", 1.0; "Yltx", 0.0 ]

// Lots of choices regarding the flags. Explain!. ugly.

let updateEnv (key: Species) (env: State) (value: Number) =
    option {
        let! _ = Map.tryFind key env.concentrations
        return Map.add key value env.concentrations
    }

// Either change to option type or do nothing and rely on type checker. Check the src dst thing only in type checker?
let safeDiv a b =
    match b with
    | 0.0 -> None
    | _ -> Some(a / b)

let safeSqrt a = if a < 0.0 then None else Some(sqrt a)

let arithmetic (env: State) (arithmetic: Arithmetic) : State option =
    match arithmetic with
    | Ld (a, b) ->
        option {
            let! value = Map.tryFind a env.concentrations
            return { env with concentrations = Map.add b value env.concentrations }
        }
    | Add (a, b, c) ->
        option {
            let! aValue = Map.tryFind a env.concentrations
            let! bValue = Map.tryFind b env.concentrations
            return { env with concentrations = Map.add c (aValue + bValue) env.concentrations }
        }
    | Sub (a, b, c) ->
        option {
            let! aValue = Map.tryFind a env.concentrations
            let! bValue = Map.tryFind b env.concentrations
            return { env with concentrations = Map.add c (aValue - bValue) env.concentrations }

        }
    | Mul (a, b, c) ->
        option {
            let! aValue = Map.tryFind a env.concentrations
            let! bValue = Map.tryFind b env.concentrations
            return { env with concentrations = Map.add c (aValue * bValue) env.concentrations }
        }
    | Div (a, b, c) ->
        option {
            let! aValue = Map.tryFind a env.concentrations
            let! bValue = Map.tryFind b env.concentrations
            let! cValue = safeDiv aValue bValue
            return { env with concentrations = Map.add c cValue env.concentrations }
        }
    | Sqrt (a, b) ->
        option {
            let! aValue = Map.tryFind a env.concentrations
            let! bValue = safeSqrt aValue
            return { env with concentrations = Map.add b bValue env.concentrations }
        }

let rec interpretCommand (cmd: Command) (state: State) : State option =
    match cmd with
    | Ar (a) ->
        option {
            let! env = arithmetic state a
            return env
        }
    | Comp (Cmp (s1, s2)) ->
        option {
            let! flags = compare (s1, s2) state.concentrations
            return state
        }
    | Cond (con) -> Some(interpretConditional con state)

and interpretCmdList (commands: Command list) (state: State) : State =
    (state, commands)
    ||> List.fold (fun state command ->
        match interpretCommand command state with
        | None -> state
        | Some s -> s)

and interpretConditional con state =
    let concs = state.concentrations

    match con with
    | IfGT (cmds) when concs["Xgty"] = 1.0 && concs["Yltx"] = 1.0 -> interpretCmdList cmds state
    | IfGE (cmds) when concs["Xgty"] = 1.0 -> interpretCmdList cmds state
    | IfEQ (cmds) when concs["Xgty"] = 1.0 && concs["Ygtx"] = 1.0 -> interpretCmdList cmds state
    | IfLT (cmds) when concs["Xlty"] = 1.0 && concs["Ygtx"] = 1.0 -> interpretCmdList cmds state
    | IfLE (cmds) when concs["Ygtx"] = 1.0 -> interpretCmdList cmds state
    | _ -> state


let interpretSteps (steps: Step list) (state: State) =
    (state, steps)
    ||> List.fold (fun state (Stp commands) -> interpretCmdList commands state)

let rec stateSequence steps state =
    seq {
        let s = interpretSteps steps state
        yield s
        yield! stateSequence steps s
    }

let interpret ((Crn (c, s)): CRN) (nSteps: int) =

    let concs0 = initConcs c

    let state0 =
        { status = Running
          concentrations = concs0 }

    (s, state0) ||> stateSequence |> Seq.take nSteps |> Seq.append (seq { state0 })

let analysisIntprt (src: string) (nSteps: int) =
    match parseString src with
    | Success (ast, _, _) -> interpret ast nSteps
    | Failure (errorMsg, _, _) -> failwith ("Parsing failed: " + errorMsg)
