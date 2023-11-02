open PeanutProver.DFA

open FParsec

let ast = run FolParser.LiteralParser.parseLiteral "x = y & z"

let prover =
    match ast with
    | Success(ast, _, _) -> FolToDFA.buildProver ast
    | Failure(_, _, _) -> failwithf "Parser error!"

prover.Recognize [ [ 1; 1; 1 ]; [ 0; 0; 0 ]; [ 0; 0; 0 ] ]
|> fun x -> printfn $"{x}"

printfn $"{ast}"
