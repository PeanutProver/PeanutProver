module PeanutProver.DFA.FolToDFA

open Ast.Ast
open PeanutProver.Automata

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

let rec buildProver ast =
    match ast with
    | BareAtom a -> convertAtom a
    | Or(left, right) ->
        let dfa_left = buildProver left
        let dfa_right = buildProver right
        DFA.union dfa_left dfa_right
    | And(left, right) ->
        let dfa_left = buildProver left
        let dfa_right = buildProver right
        DFA.intersection dfa_left dfa_right
    | Not expr ->
        let dfa_expr = buildProver expr
        DFA.complement dfa_expr
    | e -> failwithf $"Unsupported literal {e}."
