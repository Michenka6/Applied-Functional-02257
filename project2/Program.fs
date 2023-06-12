// For more information see https://aka.ms/fsharp-console-apps


open Parser
open FParsec

let input = "conc[species1,42]"

match run concParser input with
| Success (result, _, _) -> printfn $"Conc : {result}"
| Failure (errorMsg, _, _) -> printfn "Parsing failed: %s" errorMsg
