module RxnsParser

open FParsec
open System
open Types

let token p = p .>> spaces

let symbol s = token (pstring s)

// A lot of these only really make matter to check for if we allow spaces between instruction and "[" ??
let reserved =
    Set
        [ "conc"
          "step"
          "rxn"
          "ld"
          "sub"
          "mul"
          "div"
          "sqrt"
          "cmp"
          "ifGT"
          "ifGE"
          "ifEQ"
          "ifLT"
          "ifLE"
          "e" ]

let isLetter c = c |> Char.IsLetter
let isDigit d = d |> Char.IsDigit

let ident: Parser<string, unit> = // only used in species right now.
    let charOrDigit c = isLetter c || isDigit c
    token (many1Satisfy2L isLetter charOrDigit "identifier")

let isReserved str s = Set.contains str s

let pinteger: Parser<int32, unit> = token pint32

let pfloat: Parser<float, unit> = token pfloat

let pSpecies: Parser<Species, unit> = // or symbol pstring thing ?
    ident >>= fun s -> if (not (isReserved s reserved)) then preturn s else pzero

let pEmpty: Parser<Expr, unit> = symbol "e" >>. preturn Empty

let pSL: Parser<Species list, unit> = pSpecies >>= fun s -> preturn ([ s ])

let pSLopt: Parser<Species list -> Species list -> Species list, unit> =
    symbol "+" >>. preturn (fun e e' -> e @ e')

let pE: Parser<Expr, unit> =
    pEmpty <|> ((chainr1 pSL pSLopt) >>= fun l -> preturn (EL l))

let pRxn: Parser<Rxns, unit> =
    between
        (symbol "rxn[")
        (symbol "]")
        (pipe3 (pE .>> symbol ",") (pE .>> symbol ",") (pfloat) (fun s1 s2 n -> Rxn(s1, s2, n)))

let pRxnL: Parser<Rxns list, unit> = pRxn >>= fun rxn -> preturn [ rxn ]

let pRxnLopt: Parser<Rxns list -> Rxns list -> Rxns list, unit> =
    symbol "," >>. preturn (fun rxn1 rxn2 -> rxn1 @ rxn2)

// "one big bowl"
let pRxnS: Parser<Rxns list, unit> = chainr1 pRxnL pRxnLopt

let pRxnLL: Parser<Rxns list list, unit> =
    between (symbol "[") (symbol "]") (pRxnS >>= fun rxnL -> preturn [ rxnL ])

let pRxnLLopt: Parser<Rxns list list -> Rxns list list -> Rxns list list, unit> =
    symbol "," >>. preturn (fun rxnL1 rxnL2 -> rxnL1 @ rxnL2)

let pRxnListList: Parser<Rxns list list, unit> = chainr1 pRxnLL pRxnLLopt

let parseRxn = run (pRxnS .>> eof)

let parseRxns = run (pRxnListList .>> eof)

(* { rxn[a, a + atmp, 1.0], rxn[atmp, e, 1.0],
rxn[b, b + btmp, 1.0], rxn[btmp, e, 1.0],
{rxn[Xgt + b, Xlt + b, 1.0], rxn[Xlt + a, Xgt + a, 0.5],
rxn[Ygt + a, Ylt + a, 1.0], rxn[Ylt + b, Ygt + b, 1.0]},
{rxn[Xgt + Xlt, Xlt + b, 1.0], rxn[b + Xlt, Xlt + Xlt, 1.0],
rxn[Xlt + Xgt, Xgt + b, 1.0], rxn[b + Xgt, Xgt + Xgt, 1.0],
rxn[Ygt + Ylt, Ylt + a, 1.0], rxn[a + Ylt, Ylt + Ylt, 1.0],
rxn[Ylt + Ygt, Ygt + a, 1.0], rxn[a + Ygt, Ygt + Ygt, 1.0]},
 } *)
