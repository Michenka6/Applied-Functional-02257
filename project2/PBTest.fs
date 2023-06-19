module PBTest 

open FsCheck
open Types 
open RxnSim

let myFloatGen = Gen.map NormalFloat.op_Explicit Arb.generate<NormalFloat>;;

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
             let! ns = Gen.listOfLength i myFloatGen
             return Map.ofList (List.zip (vs |> List.mapi (fun i s -> s + string i)) ns) }

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
         override x.Generator = myFloatGen
         override x.Shrinker f = seq [f / 2.0]
      }

   static member State() = 
      { new Arbitrary<State>() with 
        override x.Generator = myStateGen
        override x.Shrinker m = Seq.empty
      }

Arb.register<MyGenerators>() |> ignore


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
   
   