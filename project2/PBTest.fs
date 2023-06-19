module PBTest 

open FsCheck
open Types 
open RxnSim
open System

let myFloatGen = Gen.map NormalFloat.op_Explicit Arb.generate<NormalFloat>

let floatRangeGen (min, max) =
    let range = max - min
    Gen.map (fun random -> min + random * range) myFloatGen 

let charsSeqGen c1 c2 = seq {for c in c1 .. c2 do
                               yield gen {return c} }

let myCharGen = 
   Gen.frequency [(1, gen {return! Gen.oneof (charsSeqGen 'a' 'z')}) ; 
                  (1, gen {return! Gen.oneof (charsSeqGen 'A' 'Z')})]

let mySmallStringGen = 
   gen {let! i = Gen.choose (1, 4)
        let! cs = Gen.listOfLength i myCharGen
        let ss = List.map string cs
        return String.concat "" ss }

// mapi thing to make sure string are unique..        
let mySmallEnvGen  = 
       gen { let i = 7//let! i = Gen.choose (0, 5)
             let! vs = Gen.listOfLength i mySmallStringGen
             let! ns = Gen.listOfLength i myFloatGen//(floatRangeGen(1.0, 100000.0))
             //let ns = ns |> List.map (fun v -> if v > -1.0 && v < 0.0 then v-1.0 else  if v > 0.0 && v < 1.0 then v+1.0 else v)
             return Map.ofList (List.zip (vs |> List.mapi (fun i s -> s + string i)) ns ) }

let statusGen = Gen.constant(Running) 

let myStateGen = 
   gen { let! status = statusGen 
         let! concs = mySmallEnvGen
         return {status = status; concentrations = concs}
   }

// Assumes vs<>[]
let myVarGen vs = 
    gen {let! i = Gen.choose(0, List.length vs - 1)
         return vs.[i] }


type MyGenerators =
 
   static member float() =
      { new Arbitrary<float>() with
         override x.Generator = myFloatGen//floatRangeGen(1.0, 1000000000.0)
         override x.Shrinker f = seq [f / 2.0]
      }

   static member State() = 
      { new Arbitrary<State>() with 
        override x.Generator = myStateGen
        override x.Shrinker m = Seq.empty
      }

Arb.register<MyGenerators>() |> ignore

// Validate that ld crn behaves as expected 
let ldConverge (state: State) = 
   let speciesL = state.concentrations |> Map.toList |> List.map (fun (k,_) -> k)
   let A = speciesL.[0]
   let B = speciesL.[1] 
   let rxn1 = Rxn(EL([A]), EL([A;B]), 1.0) 
   let rxn2 = Rxn(EL([B]), Empty, 1.0) 
   let crn = [rxn1; rxn2]
   
   let staten = 
      simulate 0.15 crn state 
      |> Seq.take 300
      |> List.ofSeq
      |> List.last
   //printfn "%f %f" (state.concentrations[A] + state.concentrations[B]) (staten.concentrations[C])
   
   abs (state.concentrations[A] - staten.concentrations[B]) < 0.000001

let propLdConverge =
   Prop.forAll Arb.from<State> ldConverge
 
// Validate that add crn behaves as expected 
let addConverge (state: State) = 
   let speciesL = state.concentrations |> Map.toList |> List.map (fun (k,_) -> k)
   let A = speciesL.[0]
   let B = speciesL.[1]
   let C = speciesL.[2]
   let rxn1 = Rxn(EL([A]), EL([A;C]), 1.0)
   let rxn2 = Rxn(EL([B]), EL([B;C]), 1.0)
   let rxn3 = Rxn(EL([C]), Empty, 1.0) 
   let crn = [rxn1; rxn2; rxn3]
   
   let staten = 
      simulate 0.15 crn state 
      |> Seq.take 300
      |> List.ofSeq
      |> List.last
   //printfn "%f %f" (state.concentrations[A] + state.concentrations[B]) (staten.concentrations[C])
   
   abs (state.concentrations[A] + state.concentrations[B] - staten.concentrations[C]) < 0.000001

let propAddConverge =
   Prop.forAll Arb.from<State> addConverge

// Validate that add crn behaves as expected 
let subConverge (state: State) = 
   //state.concentrations |> printfn "%A"
   let speciesL = state.concentrations |> Map.toList |> List.map (fun (k,_) -> k)
   let A = speciesL.[0]
   let B = speciesL.[1]
   let C = speciesL.[2]
   let H = speciesL.[3]
   let rxn1 = Rxn(EL([A]), EL([A;C]), 1.0)
   let rxn2 = Rxn(EL([B]), EL([B;H]), 1.0)
   let rxn3 = Rxn(EL([C]), Empty, 1.0) 
   let rxn4 = Rxn(EL([C; H]), Empty, 1.0)
   //let crn = [rxn1; rxn2; rxn3; rxn4]
   let crn = [Rxn (EL [A], EL [A; C], 1.0); Rxn (EL [B], EL [B; H], 1.0); Rxn (EL [C], Empty, 1.0); Rxn (EL [C; H], Empty, 1.0)]
   //let concs = (fun (m: Concentrations) a b -> if abs(m[a]- m[b]) < 1.0 then m |> Map.add a (m[a]+1.0) else m) state.concentrations A B 
   //let state = {status = state.status; concentrations = concs}


   let staten = 
      simulate 0.001 crn state 
      |> Seq.take 5000
      |> List.ofSeq
      |> List.last
   
   printfn "actual: %f result: %f A: %f B: %f" (state.concentrations[A] - state.concentrations[B]) (staten.concentrations[C]) state.concentrations[A] state.concentrations[B]
   
   if Double.IsNaN(staten.concentrations[C]) then 
      true 
   else if state.concentrations[A] > state.concentrations[B] then
      abs ((state.concentrations[A] - state.concentrations[B]) - staten.concentrations[C]) < 1.0
   else 
      abs (0.0 - staten.concentrations[C]) < 1.0 
let propSubConverge =
   Prop.forAll Arb.from<State> subConverge
 
// Validate that mul crn behaves as expected 
let mulConverge (state: State) = 
   let speciesL = state.concentrations |> Map.toList |> List.map (fun (k,_) -> k)
   let A = speciesL.[0]
   let B = speciesL.[1]
   let C = speciesL.[2]
   let rxn1 = Rxn(EL([A;B]), EL([A;B;C]), 1.0)
   let rxn2 = Rxn(EL([C]), Empty, 1.0) 
   let crn = [rxn1; rxn2]
   
   let staten = 
      simulate 0.15 crn state 
      |> Seq.take 300
      |> List.ofSeq
      |> List.last
   //printfn "%f %f" (state.concentrations[A] * state.concentrations[B]) (staten.concentrations[C])
   
   abs (state.concentrations[A] * state.concentrations[B] - staten.concentrations[C]) < 0.000001

let propMulConverge =
   Prop.forAll Arb.from<State> mulConverge


// Validate that mul crn behaves as expected 
let divConverge (state: State) = 
   let speciesL = state.concentrations |> Map.toList |> List.map (fun (k,_) -> k)
   let A = speciesL.[0]
   let B = speciesL.[1]
   let concs = (fun (m: Concentrations) a b -> if abs(m[a]- m[b]) < 1.0 then m |> Map.add a (m[a]+1.0) else m) state.concentrations A B 
   let state = {status = state.status; concentrations = concs}
   let C = speciesL.[2]
   let rxn1 = Rxn(EL([A]), EL([A;C]), 1.0)
   let rxn2 = Rxn(EL([B;C]), EL([B]), 1.0) 
   let crn = [rxn1; rxn2]
   
   let staten = 
      simulate 0.01 crn state 
      |> Seq.take 1000
      |> List.ofSeq
      |> List.last
   
   //printfn "%f %f %f %f" (state.concentrations[A] / state.concentrations[B]) (staten.concentrations[C]) (state.concentrations[A]) (state.concentrations[B])
   
   abs (state.concentrations[A] / state.concentrations[B] - staten.concentrations[C]) < 0.000001

let propDivConverge =
   Prop.forAll Arb.from<State> divConverge
   