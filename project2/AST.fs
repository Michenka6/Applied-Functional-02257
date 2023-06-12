module AST

type Species = string
type Number = int
type Expr = E of Species * Species


type Arithmetic =
    | Ld of Species * Species
    | Add of Species * Species * Species
    | Sub of Species * Species * Species
    | Mul of Species * Species * Species
    | Div of Species * Species * Species
    | Sqrt of Species * Species

type Comparison = Cmp of Species * Species

type Modules =
    | A of Arithmetic
    | C of Comparison

type Condition =
    | GE of CommandList
    | GT of CommandList
    | EQ of CommandList
    | LE of CommandList
    | LT of CommandList

and Step = Stp of CommandList

and Command =
    | Modl of Modules
    | Cond of Condition

and CommandList =
    | Cmnd of Command
    | Cmnds of Command * CommandList

and Concentration = Cnc of Species * Number

and Root =
    | Conc of Concentration
    | S of Step

and RootList =
    | Rt of Root
    | Rts of Root * RootList


and CRN = RootList
