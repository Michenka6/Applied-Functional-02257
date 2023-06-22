// For more information see https://aka.ms/fsharp-console-apps

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
    | Success (result, _, _) -> result
    | Failure (errorMsg, _, _) -> failwith $"Parsing failed: {errorMsg}"

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

printfn "%A" gcd

match parseString gcd with
| Failure (msg, _, _) -> failwith $"Failed to parse {msg}"
| Success (ast, _, _) ->
    match checkCRN ast with
    | CRN_Error.Error err -> failwith $"ERROR: {err}"
    | CRN_Error.Ok () ->
        printfn "CRN++ program GCD is valid and typecheked"

        let states = interpret ast
        printfn $"CRN++ program GCD is interpreted {Seq.item 50 states}"

        let (state0, rxns) = compileCrn gcd
        printfn "%A" "CRN++ program GCD successfully compiled"

        runSim 0.01 rxns state0 |> genPlot line |> showPlot














//tryParse discreteCounter

// analysisTpChkr gcd |> printfn "%A"

(* analysisIntprt fac |> Seq.take 15 |> List.ofSeq |> printfn "%A"

analysisIntprt gcd |> Seq.take 6 |> List.ofSeq |>(genPlot pieceWiseLinear) |> showPlot *)

(* let rxn1 = "rxn[A+B, A+B+C, 1.0]"
let rxn2 = "rxn[C, e, 1.0]"
let crn1 = rxn1 + "," + rxn2
 *)

(* let rxn1 = "rxn[A, A+C, 1.0]"
let rxn2 = "rxn[B, B+H, 1.0]"
let rxn3 = "rxn[C, e, 1.0]"
let rxn4 = "rxn[C + H, e, 1.0]"
let crn1 = rxn1 + "," + rxn2 + "," + rxn3 + "," + rxn4
let crnList = "[" + crn1 + "], [" + crn1 + "]"
 *)
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
(* let concs0 = [ ("A", 0.0); ("B", -1.0); ("C", 2.013399836) ] |> Map.ofList
let concs1 = Map [ ("A", 168.98259384); ("B", 79.85844836); ("C", 0.0); ("H", 0.0) ]

let concs2 = Map [ ("A", 1.188387); ("B", 14.727885) ]
let concs3 = Map [ ("A", 9.667112); ("B", -2.343401) ]

let concs4 = Map [ ("A", 5.156314); ("B", 4.815980) ]

let concs5 =
    Map
        [ ("A", 14.77495397)
          ("B", 15.68251313)
          ("C", 0.6320653985)
          ("H", 4.417154998) ]

let concs6 =
    Map
        [ ("A", 3.805313656)
          ("B", 9.111459915)
          ("C", 11.68691241)
          ("H", 0.8446240563) ]

let concs7 =
    Map
        [ ("A", 35.83402222)
          ("B", 42.18073655)
          ("C", 26.9051234)
          ("H", 29.56333537) ]

let concs8 =
    Map
        [ ("A", 17.18633644)
          ("B", 17.15438298)
          ("C", 41.9526808)
          ("H", 4.878438619) ]

let concs9 =
    Map
        [ ("A", 1.409551872)
          ("B", 43.58029173)
          ("C", 48.82977052)
          ("H", 32.84529194) ]

let concs10 =
    Map
        [ ("A", 44.92594866)
          ("B", 0.2852134009)
          ("C", 49.07211809)
          ("H", 17.55527523) ]

let concs11 =
    Map
        [ ("A", 2.90755859)
          ("B", 2.094542175)
          ("C", 1.810987414)
          ("H", 1.960499073) ]

let concs12 =
    Map[("A", 2.050869946)
        ("B", 5.958709141)
        ("C", 0.2902672932)
        ("H", 6.982265491)]

let concs13 =
    Map
        [ ("A", 46.85108317)
          ("B", 14.54780924)
          ("C", 49.94202922)
          ("H", 32.17968745) ]

let concs14 =
    Map
        [ ("A", 44.53455703)
          ("B", 30.54660123)
          ("C", 3.693756209)
          ("H", 23.24591797) ]

let state0 =
    { status = Running
      concentrations = concs14 }
 *)


//runSim 0.001 crn1 state0 |> Seq.take 7000 |> List.ofSeq |> printfn "%A"

//runSim 0.05 crn1 state0 |> Seq.take 500 |> genPlotSelect line ["A"; "B"; "C"; "H"] |> showPlot
//runSim 0.20 crn1 state0 |> Seq.take 40 |> genPlotSelect line ["A"; "B"; "C"; "H"] |> showPlot
//step |> showPlot
//     [("B4", -5.889700915); ("BJF0", 4.338130408); ("BqVk5", -2.013399836);

(* Check.Quick propLdConverge
Check.Quick propAddConverge
Check.Quick propSubConverge
Check.Quick propMulConverge
Check.Quick propDivConverge
Check.Quick propSqrtConverge *)

//let facAst =

(* compileStep (Stp [Ar (Ld ("a", "atmp")); Ar (Ld ("b", "btmp")); Comp (Cmp ("a", "b"))]) |> printfn "%s\n\n"
compileStep (Stp [Ar (Ld ("a", "atmp")); Ar (Ld ("b", "btmp")); ]) |> printfn "%s\n\n" //Comp (Cmp ("a", "b"))]) |> printfn "%s"
compileStep (Stp [Ar (Ld ("a", "atmp")); Ar (Ld ("b", "btmp")); Comp (Cmp ("a", "b"))]) |> printfn "%s\n\n"

//  *)

// let prog =
//     "crn={
//       conc[ f ,3], conc[one ,1], conc[ i , 5 ],
//       step[{
//       mul[f , i , fnext ],
//       sub[ i ,one, inext ],
//       cmp[f,i]}] };"

// let prog1 =
//     "crn={
//       conc[ f ,3], conc[one ,1], conc[ i , 5 ],
//       step[{
//       cmp[f,i]}] };"

// let prog2 =
//     "crn={
//       conc[ f ,3], conc[one ,1], conc[ i , 5 ],
//       step[{
//       mul[f , i , fnext ],
//       sub[ i ,one, inext ],
//       cmp[f,i]}],
//       step[{add[inext,one,i]}] };"

// let prog3 =
//     "crn={
//       conc[ f ,3], conc[one ,1], conc[ i , 5 ],
//       step[{
//       mul[f , i , fnext ],
//       sub[ i ,one, inext ]}]};"

// let prog4 =
//     "crn={
//       conc[ f ,3], conc[one ,1], conc[ i , 5 ], conc[c, 4], conc[d, 2],
//       step[{
//       mul[f , i , fnext ],
//       sub[ i ,one, inext ]}],
//       step[{mul[i, c, inext]}]
//       };"

// let prog5 =
//     "crn={
//       conc[ f ,3], conc[one ,1], conc[ i , 5 ], conc[c, 4], conc[d, 2],
//       step[{
//       mul[f , i , fnext ],
//       sub[ i ,one, inext ]}],
//       step[{mul[c, inext, i]}]
//       };"


// let oscState = Map [ ("X1", 0.1); ("X2", 1.1); ("X3", 2.1) ]

// let oscCrn =
//     "[rxn[X1 + X2, X2+X2, 1], rxn[X2 + X3, X3+X3, 1], rxn[X3 + X1, X1+X1, 1]];"

// let oscState1 = Map [ ("X1", 1.0); ("X2", 2.0); ("X3", 3.0) ] //; ("X4", 1.0); ("X5", 2.0); ("X6", 3.0) ]

// let oscCrn1 =
//     "rxn[X1 + X2, X2+X2, 2.0], rxn[X2 + X3, X3+X3, 2.0], rxn[X3 + X1, X1+X1, 2.0]" //, rxn[X4 + X5, X5+X5, 1.0], rxn[X5 + X6, X6+X6, 1.0], rxn[X6 + X4, X4+X4, 1.0]];"



// //let src = tryParseRxn oscCrn1
// //state |> printfn "State0: %A"
// //src |> printfn "Src:\n %s"

// //src |> (fun x -> runSim 0.20 x state) |> Seq.take 500 |> genPlotSelect line ["f"; "i"; "fnext"; "inext"; "Xgty"; "Xlty"; "Ygtx"; "Yltx"] |> showPlot
// //src |> (fun x -> runSim 0.15 x state) |> Seq.take 500 |> genPlotSelect line ["f"; "i"; "Xgty"; "Xlty"; "Ygtx"; "Yltx"] |> showPlot
// //oscCrn1 |> (fun x -> runSim 0.001 x {status = Running; concentrations = oscState1}) |> Seq.take 2000 |> genPlotSelect line ["X1"; "X2"; "X3"; "X4"; "X5"; "X6"] |> showPlot

// (* let state, src = compileCrn prog4

// let conc0 = state.concentrations
//             |> Map.add "X1" 0.5
//             |> Map.add "X2" 0.6
//             |> Map.add "X3" 0.7
//             |> Map.add "X4" 0.8
//             |> Map.add "X5" 0.9
//             |> Map.add "X6" 1.0


// let state_ = {status = state.status; concentrations = conc0}

// src |> printfn "Src:\n %s"
// conc0 |> printfn "conc0 %A"

// src |> (fun x -> runSim 0.01 x state_) |> Seq.take 3500 |> genPlot line |> showPlot *)

// //|> genPlotSelect line ["f"; "i"; "Xgty"; "Xlty"; "Ygtx"; "Yltx"; "X1"; "X2"; "X3"; "fnext"; "inext"] |> showPlot



// // let state, src = compileCrn prog5
// // state.concentrations |> Map.toList |> printfn "state?\n %A\n"
// // src |> printfn "fac?:\n %s"

// // src |> (fun x -> runSim 0.001 x state) |> Seq.take 10000 |> genPlot line |> showPlot


// (*
//       Questions
//             ok??
//             Conditionals and flags. Jump out of nothing. Compiling compare..
//             Compile into reactions. we made a LOT of choices...





// *)

// //let concs = Map [("A", 10.0); ("B", 0.0)]

// (* let speciesL = concs |> Map.toList |> List.map (fun (k, _) -> k)
// let A = speciesL.[0]
// let B = speciesL.[1]
// let rxn1 = Rxn(EL([ A ]), EL([ A; B ]), 1.0)
// let rxn2 = Rxn(EL([ B ]), Empty, 1.0)
// let crn = [ rxn1; rxn2 ]

// let state = {status = Running; concentrations = concs}

// let staten = sim 0.15  crn state |> Seq.take 60 |> genPlot line |> showPlot *)



// (* let rxn1 = Rxn(EL([ A ]), EL([ A; C ]), 1.0)
// let rxn2 = Rxn(EL([ B ]), EL([ B; C ]), 1.0)
// let rxn3 = Rxn(EL([ C ]), Empty, 1.0)
// let crn = [ rxn1; rxn2; rxn3 ]
// let state = {status = Running; concentrations = concs}

// let staten = sim 0.15 crn state |> Seq.take 50 |> genPlot line |> showPlot
//  *)
// //let concs = Map [("A", 12.0); ("B", 2.0); ("C", 0.0); ("H", 0.0)]
// (* let speciesL = concs |> Map.toList |> List.map (fun (k, _) -> k)
// let A = speciesL.[0]
// let B = speciesL.[1]
// let C = speciesL.[2]
// let H = speciesL.[3]

//  *)
// (* let rxn1 = Rxn(EL([ A ]), EL([ A; C ]), 1.0)
// let rxn2 = Rxn(EL([ B ]), EL([ B; H ]), 1.0)
// let rxn3 = Rxn(EL([ C ]), Empty, 1.0)
// let rxn4 = Rxn(EL([ C; H ]), Empty, 1.0)

// let crn = [ rxn1; rxn2; rxn3; rxn4 ] *)
// //let state = {status = Running; concentrations = concs}
// //let staten = sim 0.15 crn state |> Seq.take 200 |> genPlot line |> showPlot



// (* let speciesL = state.concentrations |> Map.toList |> List.map (fun (k, _) -> k)
// let A = speciesL.[0]
// let B = speciesL.[1]
// let C = speciesL.[2]
// let rxn1 = Rxn(EL([ A; B ]), EL([ A; B; C ]), 1.0)
// let rxn2 = Rxn(EL([ C ]), Empty, 1.0)
// let crn = [ rxn1; rxn2 ] *)




// (* let speciesL = state.concentrations |> Map.toList |> List.map (fun (k, _) -> k)
// let A = speciesL.[0]
// let B = speciesL.[1]
// let C = speciesL.[2]

// let rxn1 = Rxn(EL([ A ]), EL([ A; C ]), 1.0)
// let rxn2 = Rxn(EL([ B; C ]), EL([ B ]), 1.0)
// let crn = [ rxn1; rxn2 ]
//  *)
// (* let speciesL = state.concentrations |> Map.toList |> List.map (fun (k, _) -> k)
// let A = speciesL.[0]
// let B = speciesL.[1]
// let rxn1 = Rxn(EL([ A ]), EL([ A; B ]), 1.0)
// let rxn2 = Rxn(EL([ B; B ]), Empty, 0.5)
// let crn = [ rxn1; rxn2 ]


// sim 0.01 crn state |> Seq.take 300 |> genPlotSelect line ["A"; "B"] |> showPlot *)



// let state, src = compileCrn gcd

// src |> printfn "Src:\n %s"
// state |> printfn "state0 %A"

// src
// |> (fun x -> runSim 0.0001 x state)
// |> Seq.take 3500
// |> genPlot line
// |> showPlot
