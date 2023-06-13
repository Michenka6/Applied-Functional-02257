module Parser

open FParsec
open System
open AST

let token p = p .>> spaces

let rerserved = Set.empty |> Set.add "," |> Set.add "[" |> Set.add "]" |> Set.add "conc" |> Set.add "step" 
let isLetter c = c |> Char.IsLetter
let isDigit d = d |> Char.IsDigit


// conc[〈species〉,〈number 〉]
let speciesParser: Parser<Species, unit> = // or symbol pstring thing ?
    token (manySatisfy isLetter |>> String.Concat)

 
let intParser: Parser<Number, unit> = token pint32
let floatParser: Parser<Number, unit> = token pfloat 



let concParser: Parser<Concentration, unit> =
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
