module PBTest

open FsCheck
open Types
open RxnSim
open System

let myFloatGen = Gen.map NormalFloat.op_Explicit Arb.generate<NormalFloat>

let floatRangeGen (min, max) =
    let range = max - min
    Gen.map (fun random -> min + random * range) myFloatGen

let myFloatGen' = Gen.map2 (fun i j -> float i + j) (Arb.generate<int>) (myFloatGen)
let myFloatGen1 = Gen.map (fun x -> abs x) (myFloatGen)

let charsSeqGen c1 c2 =
    seq {
        for c in c1..c2 do
            yield gen { return c }
    }

let myCharGen =
    Gen.frequency
        [ (1, gen { return! Gen.oneof (charsSeqGen 'a' 'z') })
          (1, gen { return! Gen.oneof (charsSeqGen 'A' 'Z') }) ]

let mySmallStringGen =
    gen {
        let! i = Gen.choose (1, 4)
        let! cs = Gen.listOfLength i myCharGen
        let ss = List.map string cs
        return String.concat "" ss
    }

// mapi thing to make sure string are unique..
let mySmallEnvGen =
    gen {
        let i = 7 //let! i = Gen.choose (0, 5)
        let! vs = Gen.listOfLength i mySmallStringGen
        let! ns = Gen.listOfLength i myFloatGen1 //(floatRangeGen(1.0, 100000.0))
        //let ns = ns |> List.map (fun v -> if v > -1.0 && v < 0.0 then v-1.0 else  if v > 0.0 && v < 1.0 then v+1.0 else v)
        return Map.ofList (List.zip (vs |> List.mapi (fun i s -> s + string i)) ns)
    }

let statusGen = Gen.constant (Running)

let myStateGen =
    gen {
        let! status = statusGen
        let! concs = mySmallEnvGen

        return
            { status = status
              concentrations = concs }
    }

// Assumes vs<>[]
let myVarGen vs =
    gen {
        let! i = Gen.choose (0, List.length vs - 1)
        return vs.[i]
    }

type MyGenerators =
    static member float() =
        { new Arbitrary<float>() with
            override x.Generator = myFloatGen //floatRangeGen(1.0, 1000000000.0)
            override x.Shrinker f = seq [ f / 2.0 ] }

    static member State() =
        { new Arbitrary<State>() with
            override x.Generator = myStateGen
            override x.Shrinker m = Seq.empty }

Arb.register<MyGenerators> () |> ignore

// Validate that ld crn behaves as expected
let ldConverge (state: State) =
    let speciesL = state.concentrations |> Map.toList |> List.map (fun (k, _) -> k)
    let A = speciesL.[0]
    let B = speciesL.[1]
    let rxn1 = Rxn(EL([ A ]), EL([ A; B ]), 1.0)
    let rxn2 = Rxn(EL([ B ]), Empty, 1.0)
    let crn = [ rxn1; rxn2 ]

    let staten = simulate 0.15 crn state |> Seq.take 300 |> List.ofSeq |> List.last
    //printfn "%f %f" (state.concentrations[A] + state.concentrations[B]) (staten.concentrations[C])

    abs (state.concentrations[A] - staten.concentrations[B]) < 0.000001

let propLdConverge = Prop.forAll Arb.from<State> ldConverge

// Validate that add crn behaves as expected
let addConverge (state: State) =
    let speciesL = state.concentrations |> Map.toList |> List.map (fun (k, _) -> k)
    let A = speciesL.[0]
    let B = speciesL.[1]
    let C = speciesL.[2]
    let rxn1 = Rxn(EL([ A ]), EL([ A; C ]), 1.0)
    let rxn2 = Rxn(EL([ B ]), EL([ B; C ]), 1.0)
    let rxn3 = Rxn(EL([ C ]), Empty, 1.0)
    let crn = [ rxn1; rxn2; rxn3 ]

    let staten = simulate 0.15 crn state |> Seq.take 300 |> List.ofSeq |> List.last
    //printfn "%f %f" (state.concentrations[A] + state.concentrations[B]) (staten.concentrations[C])

    abs (state.concentrations[A] + state.concentrations[B] - staten.concentrations[C]) < 0.000001

let propAddConverge = Prop.forAll Arb.from<State> addConverge

// Validate that add crn behaves as expected
let subConverge (state: State) =
    //state.concentrations |> printfn "%A"
    let speciesL = state.concentrations |> Map.toList |> List.map (fun (k, _) -> k)
    let A = speciesL.[0]
    let B = speciesL.[1]
    let C = speciesL.[2]
    let H = speciesL.[3]
    let rxn1 = Rxn(EL([ A ]), EL([ A; C ]), 1.0)
    let rxn2 = Rxn(EL([ B ]), EL([ B; H ]), 1.0)
    let rxn3 = Rxn(EL([ C ]), Empty, 1.0)
    let rxn4 = Rxn(EL([ C; H ]), Empty, 1.0)

    let crn = [ rxn1; rxn2; rxn3; rxn4 ]

    let Ac = state.concentrations[A]
    let Bc = state.concentrations[B]
    let Cc = state.concentrations[C]
    let Hc = state.concentrations[H]

    let staten = sim 0.01 crn state |> Seq.take 400 |> List.ofSeq |> List.last

    let Ccn = staten.concentrations[C]
    let epsilon = (1.0 / ((abs (Ac - Bc)) + 1.0) * Ac * 10.0)

    //  if Ac > Bc then
    //      printfn
    //          "A: %f B: %f Actual: %f Result: %f Diff: %f epsilon: %f"
    //          Ac
    //          Bc
    //          (Ac - Bc)
    //          (Ccn)
    //          (abs (Ac - Bc - Ccn))
    //          epsilon
    //  else
    //      printfn "A <= B. Ccn: %f" Ccn

    if abs (Ac - Bc) <= 1.0 then
        //printfn "if1 Ac - Bc: %f. Ccn: %f thing: %f" (Ac - Bc) Ccn (1.0 / (abs (Ac - Bc)))
        (abs Ccn) <= (1.0 / (abs (Ac - Bc))) * 100.0
    else if Ac > Bc then
        //printfn "if2 Ac - Bc: %f. Ccn: %f Thing: %f" (Ac - Bc) Ccn (1.0 / ( (abs (Ac - Bc)) + 1.0 ) * 2.0 * Ac  )
        ((Ac - Bc) - Ccn) < epsilon
    else if Ac <= Bc then
        //printfn "if3 Ac - Bc: %f. Ccn: %f" (Ac - Bc) Ccn
        Ccn < 10.0
    else
        true


let propSubConverge = Prop.forAll Arb.from<State> subConverge

// Validate that mul crn behaves as expected
let mulConverge (state: State) =
    let speciesL = state.concentrations |> Map.toList |> List.map (fun (k, _) -> k)
    let A = speciesL.[0]
    let B = speciesL.[1]
    let C = speciesL.[2]
    let rxn1 = Rxn(EL([ A; B ]), EL([ A; B; C ]), 1.0)
    let rxn2 = Rxn(EL([ C ]), Empty, 1.0)
    let crn = [ rxn1; rxn2 ]

    let Ac = state.concentrations[A]
    let Bc = state.concentrations[B]
    let Cc = state.concentrations[C]

    let staten = simulate 0.05 crn state |> Seq.take 200 |> List.ofSeq |> List.last

    let Ccn = staten.concentrations[C]

    let epsilon = (1.0 / ((abs (Ac - Bc)) + 1.0) * 20.0)

    //  printfn "Actual: %f Result: %f Diff: %f epsilon: %f" (Ac * Bc) (Ccn) (abs (Ac * Bc - Ccn)) epsilon

    abs (Ac * Bc - Ccn) < epsilon

let propMulConverge = Prop.forAll Arb.from<State> mulConverge


// Validate that mul crn behaves as expected
let divConverge (state: State) =
    let speciesL = state.concentrations |> Map.toList |> List.map (fun (k, _) -> k)
    let A = speciesL.[0]
    let B = speciesL.[1]
    //let concs = (fun (m: Concentrations) a b -> if abs(m[a]- m[b]) < 1.0 then m |> Map.add a (m[a]+1.0) else m) state.concentrations A B
    //let state = {status = state.status; concentrations = concs}
    let C = speciesL.[2]
    let rxn1 = Rxn(EL([ A ]), EL([ A; C ]), 1.0)
    let rxn2 = Rxn(EL([ B; C ]), EL([ B ]), 1.0)
    let crn = [ rxn1; rxn2 ]

    let staten = simulate 0.05 crn state |> Seq.take 200 |> List.ofSeq |> List.last

    //printfn "%f %f %f %f" (state.concentrations[A] / state.concentrations[B]) (staten.concentrations[C]) (state.concentrations[A]) (state.concentrations[B])

    abs (state.concentrations[A] / state.concentrations[B] - staten.concentrations[C]) < 0.000001

let propDivConverge = Prop.forAll Arb.from<State> divConverge

let sqrtConverge (state: State) =
    let speciesL = state.concentrations |> Map.toList |> List.map (fun (k, _) -> k)
    let A = speciesL.[0]
    let B = speciesL.[1]
    let rxn1 = Rxn(EL([ A ]), EL([ A; B ]), 1.0)
    let rxn2 = Rxn(EL([ B; B ]), Empty, 0.5)
    let crn = [ rxn1; rxn2 ]

    if state.concentrations[A] < 0 then
        printfn "%f %f " (state.concentrations[A]) (state.concentrations[B])
        true
    else
        let staten = simulate 0.01 crn state |> Seq.take 1000 |> List.ofSeq |> List.last

        //printfn "%f %f" (sqrt state.concentrations[A]) state.concentrations[B]

        abs (sqrt state.concentrations[A] - staten.concentrations[B]) < 0.000001

let propSqrtConverge = Prop.forAll Arb.from<State> sqrtConverge
