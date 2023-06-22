module Types

(* Types for ASTs for CRN++ *)

type CRN = Crn of ConcList * StepList

and ConcList = Conc list

and StepList = Step list

and Conc = Cnc of Species * Number

and Step = Stp of CommandList

and CommandList = Command list // we could do without this type... but its okay

and Command =
    | Ar of Arithmetic
    | Comp of Comparison
    | Cond of Conditional

and Arithmetic =
    | Ld of Species * Species
    | Add of Species * Species * Species
    | Sub of Species * Species * Species
    | Mul of Species * Species * Species
    | Div of Species * Species * Species
    | Sqrt of Species * Species

and Comparison = Cmp of Species * Species

and Conditional =
    | IfGT of CommandList
    | IfGE of CommandList
    | IfEQ of CommandList
    | IfLT of CommandList
    | IfLE of CommandList

and Species = string

and Number = float


(* Types used in intepreter. State etc. *)
type Status =
    | Running
    // | Error
    | Converged

type Concentrations = Map<string, float>

type Flags =
    { Xgty: float
      Xlty: float
      Ygtx: float
      Yltx: float }

type State =
    { status: Status
      concentrations: Concentrations
    //flags: Flags
     }

(* Types used in type checker. Defines various types of errors *)
// Possibly also check all sources defined here then do no such checks in intepreter.
type CRN_Error = Result<unit, ErrorType>

and ErrorType =
    | CycleConflict
    | WriteTwice
    | SameSpeciesComparison
    | CondNoFlags
    | SrcOpNotDef

(* Types for reactions *)
type Expr =
    | Empty
    | EL of Species list

type Rxns = Rxn of Expr * Expr * float

type OptionBuilder() =
    member this.Bind(opt, func) = Option.bind func opt

    member this.Map(opt, func) = Option.map func opt

    member this.Zero() = None

    member this.Return(x) = Some x


let option = new OptionBuilder()

type ResultBuilder() =
    member this.Bind(opt, func) = Result.bind func opt

    member this.Return(x) = Ok x


let result = new ResultBuilder()
