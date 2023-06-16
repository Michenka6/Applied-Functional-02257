module RxnsParser

open FParsec
open System
open Types

let token p = p .>> spaces

let symbol s = token (pstring s)

// A lot of these only really make matter to check for if we allow spaces between instruction and "[" ??
let reserved =
    Set.empty
    |> Set.add "conc"
    |> Set.add "step"
    |> Set.add "rxn"
    |> Set.add "ld"
    |> Set.add "sub"
    |> Set.add "mul"
    |> Set.add "div"
    |> Set.add "sqrt"
    |> Set.add "cmp"
    |> Set.add "ifGT"
    |> Set.add "ifGE"
    |> Set.add "ifEQ"
    |> Set.add "ifLT"
    |> Set.add "ifLE"
    |> Set.add "e"

let isLetter c = c |> Char.IsLetter
let isDigit d = d |> Char.IsDigit

let ident: Parser<string, unit> = // only used in species right now. 
    let charOrDigit c = isLetter c || isDigit c
    token (many1Satisfy2L isLetter charOrDigit "identifier")

let isReserved str s = Set.contains str s

let pinteger: Parser<int32, unit> = token pint32

let pfloat: Parser<float, unit> = token pfloat

let pSpecies: Parser<Species, unit> = // or symbol pstring thing ?
    ident >>= fun s -> if (not (isReserved s reserved)) then preturn (Sp s) else pzero   

let pEmpty: Parser<Expr, unit> = symbol "e" >>. preturn Empty

let pSL: Parser<Species list, unit> = pSpecies >>= fun s -> preturn ([s])

let pSLopt: Parser<Species list -> Species list -> Species list,unit> = symbol "+" >>. preturn (fun e e' -> e @ e')

let pE: Parser<Expr, unit> = pEmpty <|> ((chainr1 pSL pSLopt) >>= fun l -> preturn (EL l))

let pRxn: Parser<Rxns, unit> = between (symbol "rxn[") (symbol "]") (pipe3 (pE .>> symbol ",")  (pE .>> symbol ",") (pfloat) (fun s1 s2 n -> Rxn(s1, s2, n)))

let pRxnL: Parser<Rxns list, unit> = pRxn >>= fun rxn -> preturn [rxn]
let pRxnLopt: Parser<Rxns list -> Rxns list -> Rxns list,unit> = symbol "," >>. preturn  (fun rxn1 rxn2 -> rxn1 @ rxn2)

let pRxnS: Parser<Rxns list, unit>  = chainr1 pRxnL pRxnLopt

let parseRxn = run (pRxnS  .>>  eof) 