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
