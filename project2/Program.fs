// For more information see https://aka.ms/fsharp-console-apps


open Parser
open FParsec

let input = "add[A,B,C]"

match run pArithmetic input with
| Success (result, _, _) -> printfn $"add : {result}"
| Failure (errorMsg, _, _) -> printfn "Parsing failed: %s input is: %s" errorMsg input


(* let program = "crn={
    conc[b,32 ],
    conc[a,12 ],
    step[{
    ld [a, atmp],
    ld [b, btmp],
    cmp[a,b]
    }],
    step[{
     ifGT[{ sub[atmp,btmp,a] }],
     ifLT[{ sub[btmp,atmp,b] }]
     }]
     };"

let p1 = "crn={ conc[b, 32], conc[a, 12]}"

match run crnParser p1 with 
| Success (result, _, _) -> printfn $"crn : {result}"
| Failure (errorMsg, _, _) -> printfn "Parsing failed: %s input is: %s" errorMsg program *)
