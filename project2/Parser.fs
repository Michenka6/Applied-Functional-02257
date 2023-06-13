module Parser

open FParsec
open System
open AST

(* Many of these first functions from example code given on Learn ExprParser....fsx *)
let token p = p .>> spaces
let symbol s = token (pstring s)

let reserved =
    Set.empty
    |> Set.add ","
    |> Set.add "["
    |> Set.add "]"
    |> Set.add "conc"
    |> Set.add "step"

let isLetter c = c |> Char.IsLetter
let isDigit d = d |> Char.IsDigit

let ident: Parser<string, unit> = // use in pSpecies and possibly elsewhere
    let charOrDigit c = isLetter c || isDigit c
    token (many1Satisfy2L isLetter charOrDigit "identifier")

let pinteger: Parser<int32, unit> = token pint32
let pfloat: Parser<float, unit> = token pfloat


let pInt: Parser<Number, unit> = pinteger >>= fun n -> preturn (Int n)
let pFloat: Parser<Number, unit> = pfloat >>= fun n -> preturn (Float n)
let pNumber: Parser<Number, unit> = pInt <|> pFloat

let pSpecies: Parser<Species, unit> = // or symbol pstring thing ?
    ident >>= fun s -> preturn (Sp s)

// TODO: enforce the src != dest constraint
let pSqrt: Parser<Arithmetic, unit> =
    between (symbol "sqrt[") (symbol "]") (pipe2 (pSpecies .>> pchar ',') pSpecies (fun sp1 sp2 -> Sqrt(sp1, sp2)))

let pDiv: Parser<Arithmetic, unit> =
    between
        (symbol "div[")
        (symbol "]")
        (pipe3 (pSpecies .>> pchar ',') (pSpecies .>> pchar ',') pSpecies (fun sp1 sp2 sp3 -> Div(sp1, sp2, sp3)))

let pMul: Parser<Arithmetic, unit> =
    between
        (symbol "mul[")
        (symbol "]")
        (pipe3 (pSpecies .>> pchar ',') (pSpecies .>> pchar ',') pSpecies (fun sp1 sp2 sp3 -> Mul(sp1, sp2, sp3)))

let pSub: Parser<Arithmetic, unit> =
    between
        (symbol "sub[")
        (symbol "]")
        (pipe3 (pSpecies .>> pchar ',') (pSpecies .>> pchar ',') pSpecies (fun sp1 sp2 sp3 -> Sub(sp1, sp2, sp3)))

let pAdd: Parser<Arithmetic, unit> =
    between
        (symbol "add[")
        (symbol "]")
        (pipe3 (pSpecies .>> pchar ',') (pSpecies .>> pchar ',') pSpecies (fun sp1 sp2 sp3 -> Add(sp1, sp2, sp3)))

let pLd: Parser<Arithmetic, unit> =
    between (symbol "ld[") (symbol "]") (pipe2 (pSpecies .>> pchar ',') pSpecies (fun sp1 sp2 -> Ld(sp1, sp2)))

let pArithmetic: Parser<Arithmetic, unit> =
    pLd
    <|> pAdd
    <|> pSub
    <|> pMul
    <|> pDiv
    <|> pSqrt

let pCmp: Parser<Comparison, unit> = 
    between (symbol "cmp[") (symbol "]") (pipe2 (pSpecies .>> pchar ',') pSpecies (fun sp1 sp2 -> Cmp(sp1, sp2)))


let pModule: Parser<Module, unit> =  (pArithmetic |>> fun ar -> Ar(ar)) <|> (pCmp |>> fun cmp -> Comp(cmp))

(* Slide 11 Parsing.pdf *)
let (pRootList, pRootListRef) = createParserForwardedToRef<RootList, unit>()
let (pCmdList, pCmdListRef) = createParserForwardedToRef<CommandList, unit>()

let pGT: Parser<Conditional,unit> = 
    between (symbol "ifGT[") (symbol "]") (pCmdList >>= fun l -> preturn (GT l))

let pGE: Parser<Conditional,unit> = 
    between (symbol "ifGE[") (symbol "]") (pCmdList >>= fun l -> preturn (GE l))
    
let pEQ: Parser<Conditional,unit> = 
    between (symbol "ifEQ[") (symbol "]") (pCmdList >>= fun l -> preturn (EQ l))

let pLT: Parser<Conditional,unit> = 
    between (symbol "ifLT[") (symbol "]") (pCmdList >>= fun l -> preturn (LT l))

let pLE: Parser<Conditional,unit> = 
    between (symbol "ifLE[") (symbol "]") (pCmdList >>= fun l -> preturn (LE l))

let pCond: Parser<Conditional, unit> =
    pGT 
    <|> pGE
    <|> pEQ
    <|> pLT
    <|> pLE 

let pMdl: Parser<Command, unit> = pModule >>= fun m -> preturn (Mdl m)


let pCmd: Parser<Command, unit> = pMdl <|> (pCond >>= fun cond -> preturn (Cond cond))


let pConc: Parser<Root, unit> = between (symbol "conc[") (symbol "]") (pipe2 (pSpecies .>> pchar ',') pNumber (fun sp  n -> Conc(sp, n)))

let pStep: Parser<Root, unit> = between (symbol "step[") (symbol "]") (pCmdList >>= fun l -> preturn (Step l))

let pRoot: Parser<Root, unit> = pConc <|> pStep

let pRLopt: Parser<RLopt, unit> = failwith "not implemented"

let rec pEopt e = parse { let! _ = symbol "," 
                          let! e' = pCmd                                        
                          return! pEopt(CSeq(e',e)) } 
                  <|> preturn e;;



(* let concParser: Parser<Concentration, unit> =
    between
        (token (pstring "conc["))
        (token (pchar ']'))
        (parse {
            let! species = speciesParser
            let! _ = token (pchar ',')
            let! number = numberParser
            return Cnc(species, number)
        })

// cmp [〈species〉,〈species〉]
let comparisonParser: Parser<Comparison, unit> =
    between
        (token (pstring "cmp["))
        (token (pchar ']'))
        (parse {
            let! species1 = speciesParser
            let! _ = token (pchar ',')
            let! species2 = speciesParser
            return Cmp(species1, species2)
        })

//add [〈species〉,〈species〉,〈species〉]
let addParser: Parser<Arithmetic, unit> =
    between
        (token (pstring "add["))
        (token (pstring "]"))
        (parse {
            let! species1 = speciesParser
            let! _ = token (pchar ',')
            let! species2 = speciesParser
            let! _ = token (pchar ',')
            let! species3 = speciesParser
            return Add(species1, species2, species3)
        })

//sub [〈species〉,〈species〉,〈species〉]
let subParser: Parser<Arithmetic, unit> =
    between
        (token (pstring "sub["))
        (token (pchar ']'))
        (parse {
            let! species1 = speciesParser
            let! _ = token (pchar ',')
            let! species2 = speciesParser
            let! _ = token (pchar ',')
            let! species3 = speciesParser
            return Sub(species1, species2, species3)
        })

//mul [〈species〉,〈species〉,〈species〉]
let mulParser: Parser<Arithmetic, unit> =
    between
        (token (pstring "mul["))
        (token (pchar ']'))
        (parse {
            let! species1 = speciesParser
            let! _ = token (pchar ',')
            let! species2 = speciesParser
            let! _ = token (pchar ',')
            let! species3 = speciesParser
            return Mul(species1, species2, species3)
        })

//div [〈species〉,〈species〉,〈species〉]
let divParser: Parser<Arithmetic, unit> =
    between
        (token (pstring "div["))
        (token (pchar ']'))
        (parse {
            let! species1 = speciesParser
            let! _ = token (pchar ',')
            let! species2 = speciesParser
            let! _ = token (pchar ',')
            let! species3 = speciesParser
            return Div(species1, species2, species3)
        })

//sqrt [〈species〉,〈species〉]
let sqrtParser: Parser<Arithmetic, unit> =
    between
        (token (pstring "sqrt["))
        (token (pchar ']'))
        (parse {
            let! species1 = speciesParser
            let! _ = token (pchar ',')
            let! species2 = speciesParser
            return Sqrt(species1, species2)
        })

//ld [〈species〉,〈species〉]
let loadParser: Parser<Arithmetic, unit> =
    between
        (token (pstring "ld["))
        (token (pchar ']'))
        (parse {
            let! species1 = speciesParser
            let! _ = token (pchar ',')
            let! species2 = speciesParser
            return Ld(species1, species2)
        })

let arithmeticParser: Parser<Arithmetic, unit> =
    addParser
    <|> subParser
    <|> loadParser
    <|> mulParser
    <|> divParser
    <|> sqrtParser


let rec commandListParser: Parser<CommandList, unit> =
    let geParser: Parser<Condition, unit> =
        between
            (token (pstring "ifGE["))
            (token (pchar ']'))
            (parse {
                let! c = commandListParser
                return GE c
            })

    let gtParser: Parser<Condition, unit> =
        between
            (token (pstring "ifGT["))
            (token (pchar ']'))
            (parse {
                let! c = commandListParser
                return GT c
            })

    let eqParser: Parser<Condition, unit> =
        between
            (token (pstring "ifEQ["))
            (token (pchar ']'))
            (parse {
                let! c = commandListParser
                return EQ c
            })

    let ltParser: Parser<Condition, unit> =
        between
            (token (pstring "ifLT["))
            (token (pchar ']'))
            (parse {
                let! c = commandListParser
                return LT c
            })

    let leParser: Parser<Condition, unit> =
        between
            (token (pstring "ifLE["))
            (token (pchar ']'))
            (parse {
                let! c = commandListParser
                return LE c
            })

    let rec commandParser: Parser<Command, unit> =
        (parse {
            let! cmd = arithmeticParser
            return A cmd
        })
        <|> (parse {
            let! cmd = comparisonParser
            return C cmd
        })
        <|> (parse {
            let! cmd = conditionParser
            return Cond cmd
        })

    and conditionParser: Parser<Condition, unit> =
        geParser <|> gtParser <|> eqParser <|> leParser <|> ltParser

    (parse {
        let! c = commandParser
        return Cmnd c
    })
    <|> (parse {
        let! c = commandParser
        let! _ = token (pchar ',')
        let! cs = commandListParser
        return Cmnds(c, cs)
    })

let stepParser: Parser<Step, unit> =
    between
        (token (pstring "step["))
        (token (pchar ']'))

        (parse {
            let! cmd = commandListParser
            return Stp cmd
        })

let rootParser: Parser<Root, unit> =
    (parse {
        let! c = concParser
        return Conc c
    })
    <|> (parse {
        let! s = stepParser
        return S s
    })

let rec rootListParser: Parser<RootList, unit> =
    (parse {
        let! rt = rootParser
        return Rt rt
    })
    <|> (parse {
        let! rt = rootParser
        let! _ = token (pchar ',')
        let! rts = rootListParser
        return Rts(rt, rts)
    })

let crnParser: Parser<CRN, unit> =
    between (token (pstring "crn={")) (pchar '}') rootListParser
 *)
