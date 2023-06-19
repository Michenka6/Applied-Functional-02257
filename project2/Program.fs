﻿// For more information see https://aka.ms/fsharp-console-apps

open FParsec
open FsCheck
open Types 
open Parser
open TypeChecker
open Interpreter
open PBTest
open RxnsParser
open RxnSim
open Plot 

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

//tryParse gcd

// analysisTpChkr gcd |> printfn "%A"

//analysisIntprt fac 15 |> List.ofSeq |> printfn "%A"

//analysisIntprt discreteCounter 12 |> (genPlotSelect pieceWiseLinear ["c"]) |> showPlot

let rxn1 = "rxn[A+B, A+B+C, 1.0]"
let rxn2 = "rxn[C, e, 1.0]"
let crn1 = rxn1 + "," + rxn2

// tryParseRxn rxn1
// tryParseRxn rxn2
//tryParseRxn crn1

let flags0 = { Xgty = 0.0; Xlty = 0.0; Ygtx = 0.0; Yltx = 0.0 } // initial value of flags. should not matter if well formed program
let concs0 = [("A", -5.889700915); ("B", 4.338130408); ("C", 2.013399836)] |> Map.ofList

let state0 = { status = Running; concentrations = concs0; }

//runSim 0.35 crn1 state0 |> Seq.take 25 |> List.ofSeq |> printfn "%A"

//runSim 0.15 crn1 state0 |> Seq.take 100 |> genPlot line |> showPlot
//step |> showPlot
//     [("B4", -5.889700915); ("BJF0", 4.338130408); ("BqVk5", -2.013399836);

Check.Quick propAddConverge
Check.Quick propMulConverge