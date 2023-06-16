// For more information see https://aka.ms/fsharp-console-apps

open FParsec
open Parser
open TypeChecker
open Interpreter
open RxnsParser


let tryParse s =
    match parseString s with
    | Success (result, _, _) -> printfn $"crn : {result}"
    | Failure (errorMsg, _, _) -> printfn $"Parsing failed: {errorMsg}"

let tryParseRxn s =
    match parseRxn s with
    | Success (result, _, _) -> printfn $"rxn : {result}"
    | Failure (errorMsg, _, _) -> printfn $"Parsing failed: {errorMsg}"


let gcd =
    "crn={
    conc[b,32],
    conc[a,12],
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

let p1 = "crn={ conc[b, 32], conc[a, 12]};"

let discreteCounter =
    "crn={
 conc[c,3], conc[cInitial, 3],
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

let fac =
    "crn={
 conc[ f ,1], conc[one ,1], conc[ i , 5 ],
 step[{
 cmp[i,one ],
 mul[f , i , fnext ],
 sub[ i ,one, inext ]
 }],
 step[{
 ifGT[{
 ld[ inext , i ],
 ld[ fnext , f ]
 }]
 }]
};"

tryParse gcd

// analysisTpChkr gcd |> printfn "%A"

// analysisIntprt gcd 15 |> List.ofSeq |> printfn "%A"

// let rxn1 = "rxn[A+B, A+B+C, 1.0]"
// let rxn2 = "rxn[C, e, 1.0]"
// let crn1 = rxn1 + "," + rxn2

// tryParseRxn rxn1
// tryParseRxn rxn2
// tryParseRxn crn1
