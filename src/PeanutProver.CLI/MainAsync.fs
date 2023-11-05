namespace PeanutProver.CLI

open System
open System.Collections.Generic
open Ast
open Microsoft.Extensions.Hosting
open PPlus
open FParsec
open FolParser.CommonParsers
open FolParser.LiteralParser
open PeanutProver.Automata
open PeanutProver.DFA

type Operation<'a, 'b> =
    | Def of string * string list option * Literal<'a, 'b>
    | Eval of string * string list option
    | Show of string
    | Help
    | Quit

type MainAsync(hostApplicationLifetime: IHostApplicationLifetime) =
    let _help =
        """Available commands:
* def <name>[(<bound vars)] <formula> -- Define an automaton constructed by given formula, list of bound variables is optional
* eval <name>[(<bound vars)] -- Evaluate automaton with optionally filled bound variables
* show <name> -- Show formula that was used to construct automaton
* help -- Print this message
* quit -- quit application
"""

    let _cancellationToken = hostApplicationLifetime.ApplicationStopping
    let mutable _isRunning = true
    let _automata = Dictionary<string, _>()

    let value =
        many1 (many1SatisfyL isDigit "decimal" .>> ws .>> optional (strWs ",")) .>> ws

    let parseInput =
        choice
            [ strWs "def"
              >>. tuple3 identifier (opt (between (strWs "(") (strWs ")") varList)) parseLiteral
              |>> Def
              strWs "eval" >>. tuple2 identifier (opt (between (strWs "(") (strWs ")") value))
              |>> Eval
              strWs "show" >>. identifier |>> Show
              strWs "help" >>% Help
              strWs "quit" >>% Quit ]

    let generateZeroes n = Seq.init n (fun _ -> '0')

    let doOp op =
        match op with
        | Def(name, vars, formula) ->
            // TODO: Cross check bound vars if we use DFA
            // TODO: Should we catch exceptions here at all?
            try
                let startScope =
                    vars
                    |> function
                        | None -> Ident.start
                        | Some vars -> Ident.start.Enter(vars) in

                let formula = Translate.assign startScope formula in

                let cont =
                    fun values ->
                        let formula =

                            let vars = Option.toList vars |> List.concat in
                            List.iter (printfn "var: %A") vars
                            List.iter (printfn "val: %A") values
                            let globals = List.map startScope.Look vars in
                            formula |> Translate.substituteConstant globals values in

                        FolToDFA.buildProver formula in

                _automata[name] <- (formula, cont)
            with ex ->
                PromptPlus.WriteLine(ex.Message) |> ignore

        | Eval(name, vars) ->
            match _automata.TryGetValue name with
            | true, f ->
                let result =
                    match vars with
                    | None -> (snd f <| []) |> fun x -> x.Recognize []
                    | Some vars ->
                        let lsbStrings =
                            vars
                            |> Seq.map Convert.ToInt32
                            |> Seq.map (fun x -> Convert.ToString(x, 2))
                            |> Seq.map Seq.rev

                        let longestValue = Seq.maxBy Seq.length lsbStrings |> Seq.length

                        let automatonInput =
                            lsbStrings
                            |> Seq.map (fun x ->
                                let zeroCount = longestValue - Seq.length x
                                Seq.append x (generateZeroes zeroCount))
                            |> Seq.transpose
                            |> Seq.map Seq.toList
                            |> Seq.toList

                        Seq.toList automatonInput |> (snd f) |> (fun x -> x.Recognize [])

                PromptPlus.WriteLine $"Result of {name}: {result}" |> ignore
            | false, _ -> PromptPlus.WriteLine $"Automaton with name \"{name}\" doesn't exists!" |> ignore

        | Show name ->
            match _automata.TryGetValue name with
            | true, (formula, _) -> PromptPlus.WriteLine $"{name} ⇔ {formula}" |> ignore
            | false, _ -> PromptPlus.WriteLine $"Automaton with name \"{name}\" doesn't exists!" |> ignore

        | Help -> PromptPlus.WriteLine _help |> ignore
        | Quit -> _isRunning <- false

    let rec loop () =
        let input =
            PromptPlus
                .Input("PP")
                .HistoryEnabled("InputHistory")
                .Config(fun cfg -> cfg.ShowTooltip(false) |> ignore)
                .Run(_cancellationToken)

        if input.IsAborted then
            _isRunning <- false
            ()
        else if String.IsNullOrWhiteSpace input.Value then
            loop ()
        else

            try
                let parseResult = run (parseInput .>> eof) input.Value

                match parseResult with
                | Success(operation, _, _) -> doOp operation
                | Failure(message, _, _) -> PromptPlus.WriteLine(message) |> ignore

            with e ->
                printf "Error: %s" e.Message

            if input.IsAborted then
                _isRunning <- false

            if _isRunning then
                loop ()

    member this.Run() =
        async {
            PromptPlus.Setup(fun cfg -> cfg.ColorDepth <- ColorSystem.NoColors)

            PromptPlus.DoubleDash "Welcome to Peanut Prover.\nFor help type `help`."

            loop ()

            return 0
        }
