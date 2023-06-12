module Parser

open FParsec
open System
open AST

// conc[〈species〉,〈number 〉]
let speciesParser: Parser<Species, unit> =
    manySatisfy (fun c -> not (c = ',' || c = ']')) |>> String.Concat

let numberParser: Parser<Number, unit> = spaces >>. pint32

// Main parser
let concParser: Parser<Concentration, _> =
    between
        (pstring "conc[")
        (pchar ']')
        (parse {
            let! species = speciesParser
            let! _ = pchar ','
            let! number = numberParser
            return Cnc(species, number)
        })
