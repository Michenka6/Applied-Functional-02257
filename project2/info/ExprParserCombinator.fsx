// Michael R. Hansen   09-06-2023
// A parser for simple expressions 
// - A version based on parser combinators

// This version needs explicit annotations for the parsers:
// pinteger, ident and pMinus
// The reason is explained in the documentation of FParsec:
// https://www.quanttec.com/fparsec/tutorial.html#fs-value-restriction


#r "nuget: FParsec";;

open FParsec;;

(* 
Abstract syntax based on the grammar:
    E -> V | C | ( E ) | E - E
where V is an identifier, C is an integer
*)

type E = V of string | C of int | Sub of E*E;;



// For the lexical part
// Handling terminal symbols: identifiers, integers, parantheses and '-'
// skip blanks after a token


let token p = p .>> spaces;;

let symbol s = token (pstring s);; 

// Type annotation needed to avoid value restriction error
let pinteger: Parser<int32,unit> = token pint32;; 

let ident: Parser<string,unit> = 
            let charOrDigit c = isLetter c || isDigit c
            token(many1Satisfy2L isLetter charOrDigit "identifier");;
   


let pV = ident >>= fun x -> preturn (V x);;

let pC = pinteger >>= fun n -> preturn (C n);;


// A forward reference is used to deal with the mutual recursion in the parser objects 
// pT and pE.  
// This "mechanism" is described in the 'Parsing JSON" section of the Parsec documentation
// https://www.quanttec.com/fparsec/tutorial.html

let (pE, pERef) = createParserForwardedToRef<E, unit>()    

let pT = pV 
         <|> pC 
         <|> between (symbol "(") (symbol ")") pE ;;

let pMinus: Parser<E->E->E,unit> = symbol "-" >>. preturn (fun e e' -> Sub(e,e'));;

pERef.Value <- chainl1 pT pMinus;;


// remember to skip leading spaces when parsing an expression
let parseE = spaces >>. pE;;  

// parse whole string: skip leading spaces and parse to end of input 
let parseString str = run (parseE .>> eof) str;;


let v1 = run pV "xy1   - 2";;
// val v1: ParserResult<E,unit> = Success: V "xy1"

let e1 = run parseE "(2 - x) + 5 ";;
// val e1: ParserResult<E,unit> = Success: Sub (C 2, V "x")

let e2 = parseString "(2 - x) + 5 ";;
// val e2: ParserResult<E,unit> =
//   Failure:
// Error in Ln: 1 Col: 9
// (2 - x) + 5 
//         ^
// Expecting: end of input or '-'


let e3 = parseString "   1-x";;

let e4 = parseString "   1-x-   2 ";;

let e5 = parseString "   1- ((xjhde -   2) 
  - y)
";;

