open PeanutProver.DFA

open FParsec

let ast = run FolParser.LiteralParser.parseLiteral "x < y"

let prover =
    match ast with
    | Success(ast, _, _) -> FolToDFA.buildProver ast
    | Failure(_, _, _) -> failwithf "Parser error!"

// prover.ToDot "1.dot"

prover.Recognize [ (1, 0); (0, 0); (0, 0) ] |> fun x -> printfn $"{x}"

printfn $"{ast}"
