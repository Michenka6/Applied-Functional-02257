module Parser

open FParsec
open System
open AST

// conc[〈species〉,〈number 〉]
let speciesParser: Parser<Species, unit> =
    manySatisfy (fun c -> not (c = ',' || c = ']')) |>> String.Concat

let numberParser: Parser<Number, unit> = spaces >>. pint32


let concParser: Parser<Concentration, unit> =
    between
        (pstring "conc[")
        (pchar ']')
        (parse {
            let! species = speciesParser
            let! _ = pchar ','
            let! number = numberParser
            return Cnc(species, number)
        })

// cmp [〈species〉,〈species〉]
let comparisonParser: Parser<Comparison, unit> =
    between
        (pstring "cmp[")
        (pchar ']')
        (parse {
            let! species1 = speciesParser
            let! _ = pchar ','
            let! species2 = speciesParser
            return Cmp(species1, species2)
        })

//add [〈species〉,〈species〉,〈species〉]
let addParser: Parser<Arithmetic, unit> =
    between
        (pstring "add[")
        (pstring "]")
        (parse {
            let! species1 = speciesParser
            let! _ = pchar ','
            let! species2 = speciesParser
            let! _ = pchar ','
            let! species3 = speciesParser
            return Add(species1, species2, species3)
        })

//sub [〈species〉,〈species〉,〈species〉]
let subParser: Parser<Arithmetic, unit> =
    between
        (pstring "sub[")
        (pchar ']')
        (parse {
            let! species1 = speciesParser
            let! _ = pchar ','
            let! species2 = speciesParser
            let! _ = pchar ','
            let! species3 = speciesParser
            return Sub(species1, species2, species3)
        })

//mul [〈species〉,〈species〉,〈species〉]
let mulParser: Parser<Arithmetic, unit> =
    between
        (pstring "mul[")
        (pchar ']')
        (parse {
            let! species1 = speciesParser
            let! _ = pchar ','
            let! species2 = speciesParser
            let! _ = pchar ','
            let! species3 = speciesParser
            return Mul(species1, species2, species3)
        })

//div [〈species〉,〈species〉,〈species〉]
let divParser: Parser<Arithmetic, unit> =
    between
        (pstring "div[")
        (pchar ']')
        (parse {
            let! species1 = speciesParser
            let! _ = pchar ','
            let! species2 = speciesParser
            let! _ = pchar ','
            let! species3 = speciesParser
            return Div(species1, species2, species3)
        })

//sqrt [〈species〉,〈species〉]
let sqrtParser: Parser<Arithmetic, unit> =
    between
        (pstring "sqrt[")
        (pchar ']')
        (parse {
            let! species1 = speciesParser
            let! _ = pchar ','
            let! species2 = speciesParser
            return Sqrt(species1, species2)
        })

//ld [〈species〉,〈species〉]
let loadParser: Parser<Arithmetic, unit> =
    between
        (pstring "ld[")
        (pchar ']')
        (parse {
            let! species1 = speciesParser
            let! _ = pchar ','
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
            (pstring "ifGE[")
            (pchar ']')
            (parse {
                let! c = commandListParser
                return GE c
            })

    let gtParser: Parser<Condition, unit> =
        between
            (pstring "ifGT[")
            (pchar ']')
            (parse {
                let! c = commandListParser
                return GT c
            })

    let eqParser: Parser<Condition, unit> =
        between
            (pstring "ifEQ[")
            (pchar ']')
            (parse {
                let! c = commandListParser
                return EQ c
            })

    let ltParser: Parser<Condition, unit> =
        between
            (pstring "ifLT[")
            (pchar ']')
            (parse {
                let! c = commandListParser
                return LT c
            })

    let leParser: Parser<Condition, unit> =
        between
            (pstring "ifLE[")
            (pchar ']')
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
        let! _ = pchar ','
        let! cs = commandListParser
        return Cmnds(c, cs)
    })


let rec commandParser: Parser<Command, unit> =
    let rec commandListParser: Parser<CommandList, unit> =
        (parse {
            let! c = commandParser
            return Cmnd c
        })
        <|> (parse {
            let! c = commandParser
            let! _ = pchar ','
            let! cs = commandListParser
            return Cmnds(c, cs)
        })

    let geParser: Parser<Condition, unit> =
        between
            (pstring "ifGE[")
            (pchar ']')
            (parse {
                let! c = commandListParser
                return GE c
            })

    let gtParser: Parser<Condition, unit> =
        between
            (pstring "ifGT[")
            (pchar ']')
            (parse {
                let! c = commandListParser
                return GT c
            })

    let eqParser: Parser<Condition, unit> =
        between
            (pstring "ifEQ[")
            (pchar ']')
            (parse {
                let! c = commandListParser
                return EQ c
            })

    let ltParser: Parser<Condition, unit> =
        between
            (pstring "ifLT[")
            (pchar ']')
            (parse {
                let! c = commandListParser
                return LT c
            })

    let leParser: Parser<Condition, unit> =
        between
            (pstring "ifLE[")
            (pchar ']')
            (parse {
                let! c = commandListParser
                return LE c
            })

    let conditionParser: Parser<Condition, unit> =
        geParser <|> gtParser <|> eqParser <|> leParser <|> ltParser

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

let stepParser: Parser<Step, unit> =
    between
        (pstring "step[")
        (pchar ']')

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
        let! _ = pchar ','
        let! rts = rootListParser
        return Rts(rt, rts)
    })

let parse: Parser<CRN, unit> = between (pstring "crn={") (pchar '}') rootListParser
