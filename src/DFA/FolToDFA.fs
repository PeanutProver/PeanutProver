module PeanutProver.DFA.FolToDFA

open Ast.Ast
open PeanutProver.Automata

let convertAtom atom =
    match atom with
    | Less(left, right) ->
        match left, right with
        | Var a, Var b -> PredefinedAutomata.dfa_less, [ a; b ]
        | e -> failwithf $"Unsupported: {e}"
    | Equals(left, right) ->
        match left with
        | Plus(term1, term2) ->
            match term1, term2, right with
            | Var a, Var b, Var c -> PredefinedAutomata.dfa_sum, [ a; b; c ]
            | e -> failwithf $"Unsupported: {e}"
        | Var a ->
            match right with
            | Var b -> PredefinedAutomata.dfa_eq, [ a; b ]
            | Plus(term1, term2) ->
                match term1, term2 with
                | Var b, Var c -> PredefinedAutomata.dfa_sum, [ b; c; a ]
                | e -> failwithf $"Unsupported: {e}"
            | BitwiseMinimum(term1, term2) ->
                match term1, term2 with
                | Var b, Var c -> PredefinedAutomata.dfa_bitwise_minimum, [ b; c; a ]
                | e -> failwithf $"Unsupported: {e}"
            | e -> failwithf $"Unsupported term {e}"
        | BitwiseMinimum(term1, term2) ->
            match term1, term2, right with
            | Var a, Var b, Var c -> PredefinedAutomata.dfa_bitwise_minimum, [ a; b; c ]
            | e -> failwithf $"Unsupported: {e}"
        | e -> failwithf $"Unsupported term {e}"
    | e -> failwithf $"Unsupported atom {e}"

let rec buildProver ast =
    match ast with
    | BareAtom a -> convertAtom a
    | Or(left, right) ->
        let dfa_left, left_vars = buildProver left
        let dfa_right, right_vars = buildProver right
        DFA.union dfa_left dfa_right, left_vars
    | And(left, right) ->
        let dfa_left, left_vars = buildProver left
        let dfa_right, right_vars = buildProver right
        DFA.intersection dfa_left dfa_right, left_vars
    | Not expr ->
        let dfa_expr, vars = buildProver expr
        DFA.complement dfa_expr, vars
    | e -> failwithf $"Unsupported literal {e}."
