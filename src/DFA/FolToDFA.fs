module PeanutProver.DFA.FolToDFA

open Ast
open Ast.Ast
open PeanutProver.Automata
open PeanutProver.DFA

let get_local_names formula = Passes.grab_names formula |> List.rev

let inflate_and_permute_by_formula (dfa: DFA<_>) formula number_of_vars =
    let dfa_vars_number =
        dfa.StartState.GetAllTransitions() |> List.item 0 |> fst |> List.length

    let inflated_dfa =
        DFA.inflateTransitions (number_of_vars - dfa_vars_number) false dfa

    let local_perm = get_local_names formula |> List.map Ident.id_fst

    let rec find_place element =
        let index = List.tryFindIndex (fun x -> x = element) local_perm

        match index with
        | None -> element
        | Some index -> find_place index

    let remains_perm =
        List.init (number_of_vars - local_perm.Length) (fun index -> index + local_perm.Length)
        |> List.map find_place

    (DFA.permDfa inflated_dfa (local_perm @ remains_perm))

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

let rec buildProver ast number_of_vars =
    match ast with
    | BareAtom a -> convertAtom a
    | Or(left, right) ->
        let dfa_left = buildProver left number_of_vars
        let dfa_right = buildProver right number_of_vars

        DFA.union
            (inflate_and_permute_by_formula dfa_left left number_of_vars)
            (inflate_and_permute_by_formula dfa_right right number_of_vars)
    | And(left, right) ->
        let dfa_left = buildProver left number_of_vars
        let dfa_right = buildProver right number_of_vars

        DFA.intersection
            (inflate_and_permute_by_formula dfa_left left number_of_vars)
            (inflate_and_permute_by_formula dfa_right right number_of_vars)
    | Not expr ->
        let dfa_expr = buildProver expr number_of_vars
        DFA.complement dfa_expr
    | e -> failwithf $"Unsupported literal {e}."
