// Michael R Hansen    01-06-2023

#r "nuget: FsCheck";;

open FsCheck

// Properties
let moreLazy a = a <> 0 ==> (lazy (1/a = 1/a))
Check.Quick moreLazy;;

let rec ordered = function | [] | [_] -> true
                           | x::y::rest -> x<=y && ordered(y::rest);;

let rec insert x = function | [] -> [x] 
                            | y::rest when x>y -> y::insert x rest
                            | ys               -> x::ys;;

let insertProp1 (x:int) xs = (not(ordered xs) || ordered(insert x xs))
                             |> Prop.trivial (not (ordered xs));;
Check.Quick insertProp1;;
// Ok, passed 100 tests (91% trivial).



let orderedList =  Arb.mapFilter List.sort ordered Arb.from<list<int>>;;

let insertWithArb x = Prop.forAll orderedList (fun xs -> ordered(insert x xs))

                       
Check.Quick insertWithArb;;

// Statistics about the samples


let insertTrivial (x:int) xs = 
  ordered xs ==> lazy(ordered (insert x xs))
  |> Prop.trivial (List.length xs = 0)
Check.Quick insertTrivial;;

let insertClassify (x:int) xs = 
  (not (ordered xs) || ordered (insert x xs))
  |> Prop.classify (not (ordered xs)) "false precondition"
  |> Prop.trivial (List.length xs = 0)
  |> Prop.classify (xs <> [] && ordered (x::xs)) "at-head"
  |> Prop.classify (xs <> [] && ordered (xs @ [x])) "at-tail"
  |> Prop.classify (xs <> [] && ordered xs && not (ordered (xs @ [x]) || ordered (x::xs))) "inside"

Check.Quick insertClassify;;
(*
Ok, passed 100 tests.
86% no insert.
6% at-head.
4% trivial.
2% at-tail, at-head.
2% at-tail.
val insertClassify : x:int -> xs:int list -> Property
val it : unit = ()
*)


// Gen<'a> computation expressions
// the generator that always returns x

Check.Quick (fun (x:float) y -> x+y = y+x);;  

let myFloatGen = gen { let! f = Arb.generate<NormalFloat>
                       return NormalFloat.op_Explicit f}


let myFloatGen' = Gen.map NormalFloat.op_Explicit Arb.generate<NormalFloat>;;

type E = | X 
         | C of int
         | Add of E*E;;

let rec eval x = function X -> x | C n -> n | Add(e1,e2) -> eval x e1 + eval x e2;;

let leafGen = Gen.oneof [Gen.constant X; Gen.map C Arb.generate<int>]


let rec unSafeEgen() = 
   Gen.oneof [leafGen; 
              Gen.map2 (fun x y -> Add(x,y)) (unSafeEgen()) (unSafeEgen())];;  


let _ = Check.Verbose (fun x e -> eval x e = eval x (Add(e,C 0)));; 
(*
> Stack overflow.
Repeat 19147 times:
--------------------------------
   at FSI_0012.unSafeEgen()
*)

let safeEgen() = 
   let rec myE n = match n with 
                   | 0 -> leafGen                   
                   | _ -> let egen = myE (n/2)
                          Gen.map2 (fun x y -> Add(x,y)) egen egen
   Gen.sized myE;;

let safeEgen'() = 
   let rec myE n = match n with 
                   | 0 -> leafGen                   
                   | _ -> let egen = myE (n/2)
                          Gen.oneof[leafGen;
                                    Gen.map2 (fun x y -> Add(x,y)) egen egen]
   Gen.sized myE;;

let rec subtrees e = match e with 
                     | X | C _    -> seq []
                     | Add(e1,e2) -> seq {yield! subtrees e1
                                          yield! subtrees e2
                                          yield e1
                                          yield e2}

type MyGenerators =
  static member E() =
      {new Arbitrary<E>() with
           override x.Generator = safeEgen'()
           override x.Shrinker e = subtrees e 
      }

Arb.register<MyGenerators>();;
let prop1 x e = eval x e = eval x (Add(e,C 0));; 
let prop2 x e = eval x e = eval x (Add(e,C 1));; 

