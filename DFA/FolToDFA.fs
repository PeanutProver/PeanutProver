module PeanutProver.DFA.FolToDFA

open Fol
open PeanutProver.Automata

let convertAtom (atom:Atom) =
    match atom with
    | Less (left, right) -> PredefinedAutomata.dfa_less
    | Equals (left, right) -> PredefinedAutomata.dfa_eq
    | e -> failwithf $"Unsupported atom {e}"

let rec buildProver (ast:Literal) =
    match ast with
    | BareAtom a -> convertAtom a
    | Or (left, right) ->
        let dfa_left = buildProver left
        let dfa_right = buildProver right
        DFA.union dfa_left dfa_right
    | And (left, right) ->
        let dfa_left = buildProver left
        let dfa_right = buildProver right
        DFA.intersection dfa_left dfa_right
    | Not expr ->
        let dfa_expr = buildProver expr
        DFA.complement dfa_expr
    | e -> failwithf $"Unsupported literal {e}."
