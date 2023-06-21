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
open Compiler

let tryParse s =
    match parseString s with
    | Success (result, _, _) -> printfn $"crn : {result}"
    | Failure (errorMsg, _, _) -> printfn $"Parsing failed: {errorMsg}"

let tryParseRxn s =
    match parseRxn s with
    | Success (result, _, _) -> result |> printfn "crn: %A" 
    | Failure (errorMsg, _, _) -> printfn $"Parsing failed: {errorMsg}"

let tryParseRxnLL s =
    match parseRxns s with
    | Success (result, _, _) -> result |> printfn "maybe: %A" 
    | Failure (errorMsg, _, _) -> printfn $"Parsing failed: {errorMsg}"

let tryPRxnLL s =
    match parseRxns s with
    | Success (result, _, _) -> result 
    | Failure (errorMsg, _, _) -> failwith $"Parsing failed: {errorMsg}"




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

let division = 
      "crn={
conc[a,1.0 ], conc[b,1.0 ], conc[one ,1.0],
step[{
cmp[a,b]
}],
step[{
ifGE[{
sub[a,b,anext ],
add[q,one,qnext]
}]
}],
step[{
ifGE[{
ld[anext,a ],
ld[qnext,q]
}],
ifLT[{ld[a, r ]}]
}]
};"


//tryParse discreteCounter

// analysisTpChkr gcd |> printfn "%A"

//analysisIntprt discreteCounter 15 |> List.ofSeq |> printfn "%A"

//analysisIntprt discreteCounter 12 |> (genPlotSelect pieceWiseLinear ["c"]) |> showPlot

(* let rxn1 = "rxn[A+B, A+B+C, 1.0]"
let rxn2 = "rxn[C, e, 1.0]"
let crn1 = rxn1 + "," + rxn2
 *)

let rxn1 = "rxn[A, A+C, 1.0]"
let rxn2 = "rxn[B, B+H, 1.0]"
let rxn3 = "rxn[C, e, 1.0]"
let rxn4 = "rxn[C + H, e, 1.0]"
let crn1 = rxn1 + "," + rxn2 + "," + rxn3 + "," + rxn4
let crnList = "[" + crn1 + "], [" + crn1 + "]"

//tryParseRxnLL crnList 

(* 
let sqrtRxn1 = "rxn[A, A+B, 1.0]"
let sqrtRxn2 = "rxn[B+B, e, 0.5]"

let sqrtCrn = sqrtRxn1 + "," + sqrtRxn2 *)
//tryParseRxn crn
// tryParseRxn rxn1
// tryParseRxn rxn2
//tryParseRxn crn1

//let flags0 = { Xgty = 0.0; Xlty = 0.0; Ygtx = 0.0; Yltx = 0.0 } // initial value of flags. should not matter if well formed program
//let concs0 = [("A", -5.889700915); ("B", 4.338130408); ("C", 2.013399836)] |> Map.ofList
//let concs0 = [("A", 0.02187098644); ("B", 0.8053757807); ("C", 1.194414534); ("H", 0.5613506033)] |> Map.ofList
let concs0 = [("A", 0.0); ("B", -1.0); ("C", 2.013399836)] |> Map.ofList
let concs1 = Map [("A", 168.98259384); ("B", 79.85844836); ("C", 0.0); ("H", 0.0);]

let concs2 = Map [("A", 1.188387); ("B", 14.727885)]
let concs3 = Map [("A", 9.667112); ("B", -2.343401)]

let concs4 = Map [("A", 5.156314); ("B", 4.815980)]
let concs5 = Map [("A",14.77495397); ("B",15.68251313); ("C", 0.6320653985); ("H", 4.417154998)]

let concs6 = Map [("A", 3.805313656); ("B", 9.111459915); ("C", 11.68691241); ("H", 0.8446240563);]

let concs7 = Map      [("A", 35.83402222); ("B", 42.18073655); ("C", 26.9051234);
      ("H", 29.56333537);]

let concs8 = Map [("A", 17.18633644); ("B", 17.15438298); ("C", 41.9526808);
      ("H", 4.878438619); ]

let concs9 = Map [("A", 1.409551872); ("B", 43.58029173); ("C", 48.82977052);
      ("H", 32.84529194);]

let concs10 = Map [("A", 44.92594866); ("B", 0.2852134009); ("C", 49.07211809);
      ("H", 17.55527523); ]

let concs11 =    Map [("A", 2.90755859); ("B", 2.094542175); ("C", 1.810987414);
      ("H", 1.960499073);]

let concs12 =  Map[("A", 2.050869946); ("B", 5.958709141); ("C", 0.2902672932);
      ("H", 6.982265491); ]

let concs13 = Map [("A", 46.85108317); ("B", 14.54780924); ("C", 49.94202922);
      ("H", 32.17968745); ]

let concs14 = Map [("A", 44.53455703); ("B", 30.54660123); ("C", 3.693756209);
      ("H", 23.24591797);]

let state0 = { status = Running; concentrations = concs14; }



//runSim 0.001 crn1 state0 |> Seq.take 7000 |> List.ofSeq |> printfn "%A"

//runSim 0.05 crn1 state0 |> Seq.take 500 |> genPlotSelect line ["A"; "B"; "C"; "H"] |> showPlot
//runSim 0.20 crn1 state0 |> Seq.take 40 |> genPlotSelect line ["A"; "B"; "C"; "H"] |> showPlot
//step |> showPlot
//     [("B4", -5.889700915); ("BJF0", 4.338130408); ("BqVk5", -2.013399836);
  
//Check.Quick propLdConverge
//Check.Quick propAddConverge
//Check.Quick propSubConverge
//Check.Quick propMulConverge
//Check.Quick propDivConverge
//Check.Quick propSqrtConverge

//let facAst = 

(* compileStep (Stp [Ar (Ld ("a", "atmp")); Ar (Ld ("b", "btmp")); Comp (Cmp ("a", "b"))]) |> printfn "%s\n\n"
compileStep (Stp [Ar (Ld ("a", "atmp")); Ar (Ld ("b", "btmp")); ]) |> printfn "%s\n\n" //Comp (Cmp ("a", "b"))]) |> printfn "%s"
compileStep (Stp [Ar (Ld ("a", "atmp")); Ar (Ld ("b", "btmp")); Comp (Cmp ("a", "b"))]) |> printfn "%s\n\n"

 *)

let prog =
      "crn={
      conc[ f ,3], conc[one ,1], conc[ i , 5 ],
      step[{ 
      mul[f , i , fnext ],
      sub[ i ,one, inext ],
      cmp[f,i]}] };"

let prog1 =
      "crn={
      conc[ f ,3], conc[one ,1], conc[ i , 5 ],
      step[{  
      cmp[f,i]}] };"

let prog2 =
      "crn={
      conc[ f ,3], conc[one ,1], conc[ i , 5 ],
      step[{ 
      mul[f , i , fnext ],
      sub[ i ,one, inext ],
      cmp[f,i]}],
      step[{add[inext,one,i]}] };"

let oscState = Map [("X1", 0.1); ("X2", 1.1); ("X3", 2.1)]
let oscCrn = "[rxn[X1 + X2, X2+X2, 1], rxn[X2 + X3, X3+X3, 1], rxn[X3 + X1, X1+X1, 1]];"

let oscState1 = Map [("X1", 0.1); ("X2", 0.2); ("X3", 0.3); ("X4", 0.4); ("X5", 0.5); ("X6", 0.6)]
let oscCrn1 = "[rxn[X1 + X2, X2+X2, 2.0], rxn[X2 + X3, X3+X3, 2.0], rxn[X3 + X1, X1+X1, 2.0], rxn[X4 + X5, X5+X5, 1.0], rxn[X5 + X6, X6+X6, 1.0], rxn[X6 + X4, X4+X4, 1.0]];"



//let state, src = compileCrnPP prog 
//state |> printfn "State0: %A"
//src |> printfn "Src:\n %s"

//src |> (fun x -> runSim 0.20 x state) |> Seq.take 500 |> genPlotSelect line ["f"; "i"; "fnext"; "inext"; "Xgty"; "Xlty"; "Ygtx"; "Yltx"] |> showPlot 
//src |> (fun x -> runSim 0.15 x state) |> Seq.take 500 |> genPlotSelect line ["f"; "i"; "Xgty"; "Xlty"; "Ygtx"; "Yltx"] |> showPlot 
oscCrn1 |> (fun x -> runSim 0.01 x {status = Running; concentrations = oscState1}) |> Seq.take 5500 |> genPlotSelect line ["X1"; "X2"; "X3"; "X4"; "X5"; "X6"] |> showPlot 
(*
      Questions 
            ok??
            Conditionals and flags. Jump out of nothing. Compiling compare..
            Compile into reactions. we made a LOT of choices...
            

      


*)