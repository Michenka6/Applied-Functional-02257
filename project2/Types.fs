module Types

(* Types for ASTs for CRN++ *)

type Species = string

type Number = float

type Comparison = Species * Species

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
    | Comp of Comparison
    | Cond of Conditional

and Conditional =
    | IfGT of Command list
    | IfGE of Command list
    | IfEQ of Command list
    | IfLT of Command list
    | IfLE of Command list

type Step = Step of Command list

type CRN =
    { concentrations: Concentration list
      steps: Step list }

type CRN_Error =
    | CycleConflict
    | WriteTwice
    | SameSpeciesComparison
    | CondNoFlags
    | SrcOpNotDef

(* Types used in intepreter. State etc. *)
type Status =
    | Running
    | Error
    | Converged

type Flags =
    { Xgty: bool
      Xlty: bool
      Ygtx: bool
      Yltx: bool }

type State =
    { status: Status
      concentrations: Molecules
      flags: Flags }

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
