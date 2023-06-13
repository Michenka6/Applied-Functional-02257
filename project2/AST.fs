type CRN = Crn of RootList 

and RootList = Rl of Root * Ropt

and Ropt = RSeq of RootList * Ropt | REpsilon

and Root = Conc of Species * Number | Step of CommandList // RootS ConcS and StepS merged to save derivation steps... ok? 

and CommandList = Cl of Command * Copt

and Copt =  CSeq of CommandList * Copt | CEpsilon 

and Command = Ar of Arithmetic | Comp of Comparison | Cond of Conditional

and Arithmetic = 
    | Ld of Species * Species
    | Add of Species * Species * Species
    | Sub of Species * Species * Species
    | Mul of Species * Species * Species
    | Div of Species * Species * Species
    | Sqrt of Species * Species

and Comparison = Cmp of Species * Species

and Conditional = 
    | GE of CommandList
    | GT of CommandList
    | EQ of CommandList
    | LE of CommandList
    | LT of CommandList

and Species = string 
and Number = int //Int of int | Real of float