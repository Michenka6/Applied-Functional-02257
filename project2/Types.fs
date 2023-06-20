module Types

(* Types for ASTs for CRN++ *)

type Species = string

type Number = float

type Concentration = Species * Number

type Molecules = Map<Species, Number>

type Arithmetic =
    | Ld of Species * Species
    | Add of Species * Species * Species
    | Sub of Species * Species * Species
    | Mul of Species * Species * Species
    | Div of Species * Species * Species
    | Sqrt of Species * Species

type Command =
    | Ar of Arithmetic
    | Comp of Species * Species
    | Cond of Conditional

and Conditional =
    | IfGT of Command list
    | IfGE of Command list
    | IfEQ of Command list
    | IfLT of Command list
    | IfLE of Command list

type Step = Step of Command list

type CRN =
    { molecules: Molecules
      steps: Step list }

type CRN_Error = Result<unit, ErrorType>

and ErrorType =
    | CycleConflict
    | WriteTwice
    | SameSpeciesComparison
    | CondNoFlags
    | SrcOpNotDef

(* Types used in intepreter. State etc. *)
type Status =
    | Running
    | Faulty
    | Converged

type Flags =
    { Xgty: float
      Xlty: float
      Ygtx: float
      Yltx: float }

type State =
    { status: Status
      concentrations: Molecules }

(* Types used in type checker. Defines various types of errors *)
// Possibly also check all sources defined here then do no such checks in intepreter.

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
