// For more information see https://aka.ms/fsharp-console-apps


open Parser
open FParsec

let input = "add[A,B,C]"

match run pArithmetic input with
| Success (result, _, _) -> printfn $"add : {result}"
| Failure (errorMsg, _, _) -> printfn "Parsing failed: %s input is: %s" errorMsg input


let gcd = "crn={
    conc[b,32 ],
    conc[a,12 ],
    step[{
    ld[a, atmp],
    ld[b, btmp],
    cmp[a,b]
    }],
    step[{
     ifGT[{ sub[atmp,btmp,a] }],
     ifLT[{ sub[btmp,atmp,b] }]
     }]
     };"

let p1 = "crn={ conc[b, 32], conc[a, 12]}"

match run pCrn gcd with 
| Success (result, _, _) -> printfn $"crn : {result}"
| Failure (errorMsg, _, _) -> printfn "Parsing failed: %s input is: %s" errorMsg gcd


let discreteCounter = "crn={
 conc[c,2], conc[cInitial, 4],
 conc[one ,1], conc[zero ,0],
 step[{
 sub[c,one,cnext ],
 cmp[c,zero]
 }],
 step[{
 ifGT[{ ld[cnext ,c] }],
 ifLE[{ ld[ cInitial ,c] }]
 }]
};"

match run pCrn discreteCounter with 
| Success (result, _, _) -> printfn $"crn : {result}"
| Failure (errorMsg, _, _) -> printfn "Parsing failed: %s input is: %s" errorMsg discreteCounter
