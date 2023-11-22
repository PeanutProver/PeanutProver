namespace PeanutProver.CLI

open System
open System.Collections.Generic
open Ast.Ast
open Ast
open Microsoft.Extensions.Hosting
open PPlus
open FParsec
open FolParser.CommonParsers
open FolParser.LiteralParser
open PeanutProver.DFA
open PeanutProver.DFA.Common
open PeanutProver.Automata
open Ast.Common

type Operation<'a, 'b> =
    | Def of string * string list option * Literal<'a, 'b>
    | Eval of string * string list option
    | Show of string
    | Help
    | Quit
    | Dot of string

open Ident

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
    let _automata = Dictionary<_, _>()

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
              strWs "quit" >>% Quit
              strWs "dot" >>. identifier |>> Dot ]

    let generateZeroes n = Seq.init n (fun _ -> '0')

    let doOp =
        function
        | Def(name, vars, formula) ->
            // TODO: Cross check bound vars if we use DFA
            // TODO: Should we catch exceptions here at all?
            let vars = vars |> Option.toList |> List.concat in
            // let distinct = vars |> Seq.distinct |> Seq.length in
            // if distinct != vars.Length then failwith "Vars must be distinct!"
            let startScope = vars |> (Ident.start ()).Enter in

            let formula = Passes.assignId startScope formula in
            let names = Passes.grab_names formula |> List.rev
            let global_names = List.map (startScope.Look) vars

            let permutation =
                List.mapi (fun index name -> index, List.findIndex ((=) name) names) global_names
                |> Map.ofList
                |> fun map number -> Map.find number map

            let get_name (Id(_, name)) = name

            let dfa, automation_vars = FolToDFA.buildProver formula

            let automation_indices =
                automation_vars
                |> List.map (fun i -> List.findIndex (fun var_name -> get_name i = var_name) vars)

            let new_transitions =
                renumber_transitions dfa.Transitions vars.Length automation_indices

            let dfa = DFA(dfa.StartState, dfa.FinalStates, new_transitions)

            _automata[name] <- (formula, dfa)
        | Eval(name, args) ->
            match _automata.TryGetValue name with
            | true, (_, dfa) ->

                let args =
                    args
                    |> Option.toList
                    |> List.concat
                    |> List.map Common.strToBits
                    |> List.map Seq.toList

                let max_size =
                    (0, args)
                    ||> List.fold (fun acc next -> if acc < next.Length then next.Length else acc)

                let args =
                    args
                    |> List.map (fun el -> [ List.init (max_size - el.Length) (fun _ -> Zero); el ] |> List.concat)
                    |> List.map List.rev
                    |> List.transpose in

                let result = dfa.Recognize(args)
                PromptPlus.WriteLine $"Result of {name}: {result}" |> ignore
            | false, _ -> PromptPlus.WriteLine $"Automaton with name \"{name}\" doesn't exists!" |> ignore
        | Show name ->
            match _automata.TryGetValue name with
            | true, (formula, _) -> PromptPlus.WriteLine $"{name} ⇔ {formula}" |> ignore
            | false, _ -> PromptPlus.WriteLine $"Automaton with name \"{name}\" doesn't exists!" |> ignore
        | Help -> PromptPlus.WriteLine _help |> ignore
        | Quit -> _isRunning <- false
        | Dot name ->
            match _automata.TryGetValue name with
            | true, (_, automaton) -> PromptPlus.WriteLine $"{automaton.ToDot()}" |> ignore
            | false, _ -> PromptPlus.WriteLine $"Automaton with name \"{name}\" doesn't exists!" |> ignore

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
            // try
            let parseResult = run (parseInput .>> eof) input.Value

            match parseResult with
            | Success(operation, _, _) -> doOp operation
            | Failure(message, _, _) -> PromptPlus.WriteLine(message) |> ignore

            // with e ->
            // PromptPlus.WriteLine(e.Message) |> ignore

            if input.IsAborted then
                _isRunning <- false

            if _isRunning then
                loop ()

    member this.Run() =
        async {
            PromptPlus.Setup(fun cfg -> cfg.ColorDepth <- ColorSystem.NoColors)
            PromptPlus.IgnoreColorTokens <- true

            PromptPlus.DoubleDash "Welcome to Peanut Prover.\nFor help type `help`."

            loop ()

            return 0
        }
