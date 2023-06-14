module Parser

open FParsec
open System
open AST

(*  
    TODO: 
        Decide whether whitespace allowed between [ and {, } and ]
        Decide whether whitespace allowed between instruction name and [
        Decide whether programs must end with ;
        Type checker what is this? Right now we accept or do not -- CHECK IF NO CYCLES CMP BEFORE COND ETC...
*)

(* Many of these first functions from example code given on Learn ExprParser....fsx *)
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

let isLetter c = c |> Char.IsLetter
let isDigit d = d |> Char.IsDigit

let ident: Parser<string, unit> = // only used in species right now. 
    let charOrDigit c = isLetter c || isDigit c
    token (many1Satisfy2L isLetter charOrDigit "identifier")

let isReserved str s = Set.contains str s

let pinteger: Parser<int32, unit> = token pint32

let pfloat: Parser<float, unit> = token pfloat

let pFloat: Parser<Number, unit> = pfloat >>= fun n -> preturn (n)
let pNumber: Parser<Number, unit> = pFloat //pInt <|> pFloat

let pSpecies: Parser<Species, unit> = // or symbol pstring thing ?
    ident >>= fun s -> if (not (isReserved s reserved)) then preturn (Sp s) else pzero   

// TODO: enforce the src != dest constraint
let pSqrt: Parser<Arithmetic, unit> =
    between (symbol "sqrt[") (symbol "]") (pipe2 (pSpecies .>> symbol ",") pSpecies (fun sp1 sp2 -> Sqrt(sp1, sp2)))

let pDiv: Parser<Arithmetic, unit> =
    between
        (symbol "div[")
        (symbol "]")
        (pipe3 (pSpecies .>> symbol ",") (pSpecies .>> symbol ",") pSpecies (fun sp1 sp2 sp3 -> Div(sp1, sp2, sp3)))

let pMul: Parser<Arithmetic, unit> =
    between
        (symbol "mul[")
        (symbol "]")
        (pipe3 (pSpecies .>> symbol ",") (pSpecies .>> symbol ",") pSpecies (fun sp1 sp2 sp3 -> Mul(sp1, sp2, sp3)))

let pSub: Parser<Arithmetic, unit> =
    between
        (symbol "sub[")
        (symbol "]")
        (pipe3 (pSpecies .>> symbol ",") (pSpecies .>> symbol ",") pSpecies (fun sp1 sp2 sp3 -> Sub(sp1, sp2, sp3)))

let pAdd: Parser<Arithmetic, unit> =
    between
        (symbol "add[")
        (symbol "]")
        (pipe3 (pSpecies .>> symbol ",") (pSpecies .>> symbol ",") pSpecies (fun sp1 sp2 sp3 -> Add(sp1, sp2, sp3)))

let pLd: Parser<Arithmetic, unit> =
    between (symbol "ld[") (symbol "]") (pipe2 (pSpecies .>> symbol ",") pSpecies (fun sp1 sp2 -> Ld(sp1, sp2)))

let pArithmetic: Parser<Arithmetic, unit> =
    pLd
    <|> pAdd
    <|> pSub
    <|> pMul
    <|> pDiv
    <|> pSqrt

let pCmp: Parser<Comparison, unit> = 
    between (symbol "cmp[") (symbol "]") (pipe2 (pSpecies .>> symbol ",") pSpecies (fun sp1 sp2 -> Cmp(sp1, sp2)))

(* Slide 11 Parsing.pdf *)
let (pRootList, pRootListRef) = createParserForwardedToRef<RootList, unit>()
let (pCmdList, pCmdListRef) = createParserForwardedToRef<CommandList, unit>()

let pGT: Parser<Conditional,unit> = 
    between (symbol "ifGT[{") (symbol "}]") (pCmdList >>= fun l -> preturn (IfGT l))

let pGE: Parser<Conditional,unit> = 
    between (symbol "ifGE[{") (symbol "}]") (pCmdList >>= fun l -> preturn (IfGE l))
    
let pEQ: Parser<Conditional,unit> = 
    between (symbol "ifEQ[{") (symbol "}]") (pCmdList >>= fun l -> preturn (IfEQ l))

let pLT: Parser<Conditional,unit> = 
    between (symbol "ifLT[{") (symbol "}]") (pCmdList >>= fun l -> preturn (IfLT l))

let pLE: Parser<Conditional,unit> = 
    between (symbol "ifLE[{") (symbol "}]") (pCmdList >>= fun l -> preturn (IfLE l))

let pCond: Parser<Conditional, unit> =
    pGT 
    <|> pGE
    <|> pEQ
    <|> pLT
    <|> pLE 

let pCmd: Parser<Command, unit> = (pArithmetic |>> fun ar -> Ar(ar)) <|> (pCmp |>> fun cmp -> Comp(cmp)) <|> (pCond >>= fun cond -> preturn (Cond cond)) 
let pConc: Parser<Root, unit> = between (symbol "conc[") (symbol "]") (pipe2 (pSpecies .>> symbol ",") pNumber (fun sp  n -> Conc(sp, n)))

let pStep: Parser<Root, unit> = between (symbol "step[{") (symbol "}]") (pCmdList >>= fun l -> preturn (Step l))

let pRoot: Parser<Root, unit> = 
    pConc 
    <|> pStep 

let pC: Parser<CommandList, unit> = pCmd >>= fun c -> preturn ([c]) 

let pCLopt: Parser<CommandList->CommandList->CommandList,unit> = symbol "," >>. preturn (fun c1 c2 -> c1 @ c2)

pCmdListRef.Value <- chainr1 pC pCLopt

let pR: Parser<RootList, unit> = pRoot >>= fun r -> preturn ([r])

let pRLopt: Parser<RootList->RootList->RootList,unit> = symbol "," >>. preturn (fun r1 r2 -> r1 @ r2)

pRootListRef.Value <- chainr1 pR pRLopt

let pCrn: Parser<CRN, unit> = between (spaces >>. symbol "crn={") (symbol "};") pRootList >>= fun rl -> preturn (Crn rl) 

// eof consume till end of input
let parseString = run (pCrn  .>>  eof) 