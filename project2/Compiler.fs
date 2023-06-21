module Compiler

open Types

let getFromCnd =
    function
    | IfGE cl -> cl
    | IfGT cl -> cl
    | IfEQ cl -> cl
    | IfLE cl -> cl
    | IfLT cl -> cl

let compileA =
    function
    | Ld (a, b) -> $"[ rxn[{a}, {a} + {b}, 1.0], rxn[{b}, e, 1.0] ]\n"
    | Add (a, b, c) -> $"[ rxn[{a}, {a} + {c}, 1.0], rxn[{b}, {b} + {c}, 1.0], rxn[{c}, e, 1.0] ]\n"
    | Sub (a, b, c) -> $"[ rxn[{a}, {a} + {c}, 1.0], rxn[{b}, {b} + H, 1.0], rxn[{c}, e, 1.0], rxn[{c}, e, 1.0] ]\n"
    | Mul (a, b, c) -> $"[ rxn[{a} + {b}, {a} + {b} + {c}, 1.0], rxn[{c}, e, 1.0] ]\n"
    | Div (a, b, c) -> $"[ rxn[{a}, {a} + {c}, 1.0], rxn[{b} + {c}, {b}, 1.0] ]\n"
    | Sqrt (a, b) -> $"[ rxn[{a}, {a} + {b}, 1.0], rxn[{b} + {b}, e, 1.0/2] ]\n"

let compileComp x y =
    $" [ rxn[Xg + {y}, Xl + {y}, 1.0], rxn[Xl + {x}, Xg + {x}, 1.0], rxn[Yg + {x}, Yl + {x}, 1.0], rxn[Yl + {y}, Yg + {y}, 1.0] ],\n[ rxn[Xg + Yl, Xl, + {y}, 1.0], rxn[{y} + Xl, Xl + Xl, 1.0], rxn[Xl + Xg, Xg + {y}, 1.0], rxn[{y} + Xg, Xg + Xg, 1.0] ]"

let rec compileCond cond =
    let cl = getFromCnd cond
    compileCL cl

and compileCL =
    function
    | [] -> ""
    | [ Ar a ] -> $"{compileA a}"
    | [ Cond c ] -> $"{compileCond c}"
    | [ Comp (a, b) ] -> $"{compileComp a b}"
    | Ar a :: tail -> $"{compileA a}, {compileCL tail}"
    | Comp (a, b) :: tail -> $"{compileComp a b}, {compileCL tail}"
    | Cond c :: tail -> $"{compileCond c}, {compileCL tail}"

let compileStep (Step cl) = compileCL cl

let compileCRN crn =
    crn.steps |> List.map compileStep |> String.concat "\n," |> (fun x -> $"[{x}]")
