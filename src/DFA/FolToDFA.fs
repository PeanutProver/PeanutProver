module PeanutProver.DFA.FolToDFA

open Ast.Ast
open PeanutProver.Automata
open PeanutProver.DFA.Common

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

let removeRepetitions (dfa: DFA) vars =
    let new_vars = vars |> Seq.distinct |> List.ofSeq
    let var_number = new_vars.Length
    let indices = List.map (fun var -> List.findIndex ((=) var) new_vars) vars
    let new_transitions = renumber_transitions dfa.Transitions var_number indices
    DFA(dfa.StartState, dfa.FinalStates, new_transitions), new_vars

let makeCompatible transitions1 vars1 transitions2 vars2 =
    let new_vars = [ vars1; vars2 ] |> List.concat |> Seq.distinct |> List.ofSeq
    let var_number = new_vars.Length
    let indices1 = List.map (fun var -> List.findIndex ((=) var) new_vars) vars1
    let indices2 = List.map (fun var -> List.findIndex ((=) var) new_vars) vars2
    let transitions1 = renumber_transitions transitions1 var_number indices1
    let transitions2 = renumber_transitions transitions2 var_number indices2
    transitions1, transitions2, new_vars

let rec buildProver ast =
    match ast with
    | BareAtom a -> convertAtom a ||> removeRepetitions
    | Or(left, right) ->
        let dfa_left, left_vars = buildProver left
        let dfa_right, right_vars = buildProver right

        let transitions_left, transitions_right, new_vars =
            makeCompatible dfa_left.Transitions left_vars dfa_right.Transitions right_vars

        let new_dfa_left = DFA(dfa_left.StartState, dfa_left.FinalStates, transitions_left)

        let new_dfa_right =
            DFA(dfa_right.StartState, dfa_right.FinalStates, transitions_right)

        DFA.union new_dfa_left new_dfa_right, new_vars
    | And(left, right) ->
        let dfa_left, left_vars = buildProver left
        let dfa_right, right_vars = buildProver right

        let transitions_left, transitions_right, new_vars =
            makeCompatible dfa_left.Transitions left_vars dfa_right.Transitions right_vars

        let new_dfa_left = DFA(dfa_left.StartState, dfa_left.FinalStates, transitions_left)

        let new_dfa_right =
            DFA(dfa_right.StartState, dfa_right.FinalStates, transitions_right)

        DFA.intersection new_dfa_left new_dfa_right, new_vars
    | Not expr ->
        let dfa, vars = buildProver expr
        DFA.complement dfa, vars
    | Exists(names, expr) ->
        let dfa, vars = buildProver expr
        let indices_to_squash = List.map (fun var -> List.findIndex ((=) var) vars) names
        let dfa = DFA.projection dfa indices_to_squash
        let vars = List.filter (fun x -> not <| List.exists ((=) x) names) vars
        dfa, vars
    | e -> failwithf $"Unsupported literal {e}."
