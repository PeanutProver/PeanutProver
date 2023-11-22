module PeanutProver.DFA.FolToDFA

open Ast.Ast
open PeanutProver.Automata
open PeanutProver.DFA

let convertAtom atom =
    match atom with
    | Less(left, right) -> PredefinedAutomata.dfa_less
    | Equals(left, right) ->
        match left with
        | Plus(term1, term2) -> PredefinedAutomata.dfa_sum
        | Var a ->
            match right with
            | Var a -> PredefinedAutomata.dfa_eq
            | Plus(term1, term2) -> PredefinedAutomata.dfa_sum
            | BitwiseMinimum(term1, term2) -> PredefinedAutomata.dfa_bitwise_minimum
            | e -> failwithf $"Unsupported term {e}"
        | BitwiseMinimum(term1, term2) -> PredefinedAutomata.dfa_bitwise_minimum
        | e -> failwithf $"Unsupported term {e}"

    | e -> failwithf $"Unsupported atom {e}"

let rec buildProver ast numberOfVars =
    match ast with
    | BareAtom a -> convertAtom a
    | Or(left, right) ->
        let dfa_left = buildProver left numberOfVars
        let dfa_right = buildProver right numberOfVars
        let dfa_left_vars_number = dfa_left.StartState.GetAllTransitions() |> List.item 0 |> fst |> List.length
        let dfa_right_vars_number = dfa_right.StartState.GetAllTransitions() |> List.item 0 |> fst |> List.length
        DFA.union
            (DFA.inflateTransitions (numberOfVars - dfa_left_vars_number) false dfa_left)
            (DFA.inflateTransitions (numberOfVars - dfa_right_vars_number) true dfa_right)
    | And(left, right) ->
        let dfa_left = buildProver left numberOfVars
        let dfa_right = buildProver right numberOfVars
        let dfa_left_vars_number = dfa_left.StartState.GetAllTransitions() |> List.item 0 |> fst |> List.length
        let dfa_right_vars_number = dfa_right.StartState.GetAllTransitions() |> List.item 0 |> fst |> List.length
        DFA.intersection
            (DFA.inflateTransitions (numberOfVars - dfa_left_vars_number) false dfa_left)
            (DFA.inflateTransitions (numberOfVars - dfa_right_vars_number) true  dfa_right)
    | Not expr ->
        let dfa_expr = buildProver expr numberOfVars
        DFA.complement dfa_expr
    | e -> failwithf $"Unsupported literal {e}."
