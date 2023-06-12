// For more information see https://aka.ms/fsharp-console-apps


open Parser
open FParsec

let input = "add[A,B,C]"

match run arithmeticParser input with
| Success (result, _, _) -> printfn $"add : {result}"
| Failure (errorMsg, _, _) -> printfn "Parsing failed: %s input is: %s" errorMsg input
