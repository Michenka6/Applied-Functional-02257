// Michael R. Hansen   10-06-2023

// Generators for
// - small strings
// - small environments
// - closed expressions (having no free variables)

#r "nuget: FsCheck";;

open FsCheck

type E =  | V of string 
          | C of int
          | Let of string * E * E 
          | Add of E * E;;

type Env = Map<string,int>;;

let rec free = 
      function 
      | C _          -> Set.empty
      | V x          -> Set.singleton x
      | Add(e1,e2)   -> Set.union (free e1) (free e2)
      | Let(x,e1,e2) -> Set.union (free e1) (Set.remove x (free e2));;

let closed e = free e = Set.empty;;

//eval: E -> Env -> int
let rec eval e m = 
    match e with 
    | V x -> Map.find x m 
    | C n -> n
    | Let(x,e1,e) -> let v1 = eval e1 m 
                     eval e (Map.add x v1 m)
    | Add(e1,e2)  -> eval e1 m + eval e2  m;; 


// For all closed e, for all m: eval e m = eval e Map.empty

let closedProp e m = eval e m = eval e Map.empty;;

// Generation of small strings, small environments and closed expressions

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

Gen.sample 5 4 mySmallStringGen;; 

let mySmallEnvGen  = 
       gen { let! i = Gen.choose (0, 5)
             let! vs = Gen.listOfLength i mySmallStringGen
             let! ns = Gen.listOfLength i Arb.generate<int>
             return Map.ofList (List.zip vs ns)      };;

// Assumes vs<>[]
let myVarGen vs = 
    gen {let! i = Gen.choose(0, List.length vs - 1)
         return vs.[i] }

Gen.sample 2 4 mySmallEnvGen  ;; 

let myCGen    = Gen.map C Arb.generate<int>;; 

let myVGen vs = Gen.map V (myVarGen vs)

let myLeafGen vs = 
   if vs<>[] 
   then Gen.oneof [myCGen ; 
                   myVGen vs]
   else myCGen;;

// Generator of closed expressions
let myEGen = 
   let rec myE vs n = 
      match n with 
      | 0 -> myLeafGen vs 
      | _ -> Gen.oneof [myLeafGen vs;
                        Gen.map2 (fun x y -> Add(x,y)) (myE vs (n/2)) (myE vs (n/2));
                        myLetGen vs n                                               ]
   and myLetGen vs n = gen {let! x  = mySmallStringGen 
                            let! e1  = myE vs (n/2) 
                            let! e = myE (x::vs) (n/2)
                            return Let(x,e1,e) } 
   Gen.sized (myE []);;



type MyGenerator =
  static member Env() =
      {new Arbitrary<Map<string,int>>() with
          override x.Generator = mySmallEnvGen
          override x.Shrinker t = Seq.empty }
  static member E() =
       {new Arbitrary<E>() with
           override x.Generator = myEGen 
           override x.Shrinker t = Seq.empty };;

Arb.register<MyGenerator>();;

let _ = Check.Verbose (fun e -> free e = Set.empty);;

let _ = Check.Verbose closedProp;;

