module AST

type CRN = Crn of ConcList * StepList 

and ConcList = Conc list 
and StepList = Step list 
and Conc = Cnc of Species * Number 
and Step = Stp of CommandList 
//and RootList = Root list 

//and Root = Conc of Species * Number | Step of CommandList 

and CommandList = Command list // we could do without this type... but its okay  

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
    | IfGT of CommandList
    | IfGE of CommandList
    | IfEQ of CommandList
    | IfLT of CommandList
    | IfLE of CommandList

and Species = Sp of string  

and Number = float 