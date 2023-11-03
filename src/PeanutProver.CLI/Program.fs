open PeanutProver.Automata
open PeanutProver.DFA

open FParsec

let ast = run FolParser.LiteralParser.parseLiteral "x = y & z"

let prover =
    match ast with
    | Success(ast, _, _) -> FolToDFA.buildProver ast
    | Failure _ -> failwithf "Parser error!"

prover.Recognize [ [ One; One ; One ]; [ Zero; Zero; Zero ]; [ Zero; Zero; Zero ] ]
|> fun x -> printfn $"{x}"

printfn $"{ast}"
