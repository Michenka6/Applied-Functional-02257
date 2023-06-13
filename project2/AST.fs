module AST

type CRN = Crn of RootList 

//and RootList = RL of Root * RLopt
and RootList = R of Root | RL of RootList * RootList
//and RLopt = RSeq of Root * RLopt | REpsilon

and Root = Conc of Species * Number | Step of CommandList // RootS ConcS and StepS merged to save derivation steps... ok? 

//and CommandList = CL of Command * CLopt
and CommandList = C of Command | CL of CommandList * CommandList 
//and CLopt =  CSeq of Command * CLopt | CEpsilon 

and Command = Mdl of Module | Cond of Conditional

and Module = Ar of Arithmetic | Comp of Comparison  // Skip module syntactic category as type? instead seems cleaner...

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

and Number = Int of int | Float of float