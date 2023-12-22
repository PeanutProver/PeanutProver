module PeanutProver.NFA.FolToNFA

open System.Collections.Generic
open Ast.Ast
open PeanutProver.Automata
open PeanutProver.NFA.Common

let convertAtom atom (_automata: Dictionary<_, _>) =
    match atom with
    | Less(left, right) ->
        match left, right with
        | Var a, Var b -> PredefinedAutomata.nfa_less, [ a; b ]
        | e -> failwithf $"Unsupported: {e}"
    | Equals(left, right) ->
        match left with
        | Plus(term1, term2) ->
            match term1, term2, right with
            | Var a, Var b, Var c -> PredefinedAutomata.nfa_sum, [ a; b; c ]
            | e -> failwithf $"Unsupported: {e}"
        | Var a ->
            match right with
            | Var b -> PredefinedAutomata.nfa_eq, [ a; b ]
            | Plus(term1, term2) ->
                match term1, term2 with
                | Var b, Var c -> PredefinedAutomata.nfa_sum, [ b; c; a ]
                | e -> failwithf $"Unsupported: {e}"
            | BitwiseMinimum(term1, term2) ->
                match term1, term2 with
                | Var b, Var c -> PredefinedAutomata.nfa_bitwise_minimum, [ b; c; a ]
                | e -> failwithf $"Unsupported: {e}"
            | Const b -> PredefinedAutomata.fa_constant_eq b, [ a ]
            | e -> failwithf $"Unsupported term {e}"
        | BitwiseMinimum(term1, term2) ->
            match term1, term2, right with
            | Var a, Var b, Var c -> PredefinedAutomata.nfa_bitwise_minimum, [ a; b; c ]
            | e -> failwithf $"Unsupported: {e}"
        | Const a ->
            match right with
            | Var b -> PredefinedAutomata.fa_constant_eq a, [ b ]
            | e -> failwithf $"Unsupported: {e}"
        | e -> failwithf $"Unsupported term {e}"
    | Automaton(name, terms) ->
        match _automata.TryGetValue name with
        | true, (_, automaton) ->
            automaton,
            (terms
             |> List.map (fun t ->
                 match t with
                 | Var a -> a
                 | _ -> failwithf $"Automaton {name} can only accept vars"))
        | false, _ -> failwithf $"Automaton {name} is not found"

    | True -> failwith "True is not implemented"
    | False -> failwith "False is not implemented"
    | Greater(term, term1) -> failwith "> is not implemented"

let removeRepetitions (nfa: NFA) vars =
    let new_vars = vars |> Seq.distinct |> List.ofSeq
    let var_number = new_vars.Length
    let indices = List.map (fun var -> List.findIndex ((=) var) new_vars) vars
    let new_transitions = renumber_transitions nfa.Transitions var_number indices
    NFA(nfa.StartState, nfa.FinalStates, new_transitions), new_vars

let makeCompatible transitions1 vars1 transitions2 vars2 =
    let new_vars = [ vars1; vars2 ] |> List.concat |> Seq.distinct |> List.ofSeq
    let var_number = new_vars.Length
    let indices1 = List.map (fun var -> List.findIndex ((=) var) new_vars) vars1
    let indices2 = List.map (fun var -> List.findIndex ((=) var) new_vars) vars2
    let transitions1 = renumber_transitions transitions1 var_number indices1
    let transitions2 = renumber_transitions transitions2 var_number indices2
    transitions1, transitions2, new_vars

let rec buildProver ast _automata =
    match ast with
    | BareAtom a ->
        convertAtom a _automata
        ||> removeRepetitions
        |> (fun (nfa, s) -> NFA.minimization nfa, s)
    | Or(left, right) ->
        let nfa_left, left_vars = buildProver left _automata
        let nfa_right, right_vars = buildProver right _automata

        let transitions_left, transitions_right, new_vars =
            makeCompatible nfa_left.Transitions left_vars nfa_right.Transitions right_vars

        let new_nfa_left = NFA(nfa_left.StartState, nfa_left.FinalStates, transitions_left)

        let new_nfa_right =
            NFA(nfa_right.StartState, nfa_right.FinalStates, transitions_right)

        NFA.union new_nfa_left new_nfa_right |> NFA.minimization, new_vars
    | And(left, right) ->
        let nfa_left, left_vars = buildProver left _automata
        let nfa_right, right_vars = buildProver right _automata

        let transitions_left, transitions_right, new_vars =
            makeCompatible nfa_left.Transitions left_vars nfa_right.Transitions right_vars

        let new_nfa_left = NFA(nfa_left.StartState, nfa_left.FinalStates, transitions_left)

        let new_nfa_right =
            NFA(nfa_right.StartState, nfa_right.FinalStates, transitions_right)

        NFA.intersection new_nfa_left new_nfa_right |> NFA.minimization, new_vars
    | Not expr ->
        let nfa, vars = buildProver expr _automata
        nfa.ToDFA () |> NFA.complement , vars
    | Exists(names, expr) ->
        let nfa, vars = buildProver expr _automata
        let indices_to_squash = List.map (fun var -> List.findIndex ((=) var) vars) names
        let nfa = NFA.projection nfa indices_to_squash
        let vars = List.filter (fun x -> not <| List.exists ((=) x) names) vars
        nfa.ToDFA () |> NFA.quotientZero, vars
    | Forall(names, expr) ->
        let nfa, vars = buildProver (Not expr) _automata
        let nfa = nfa |> NFA.complement
        let indices_to_squash = List.map (fun var -> List.findIndex ((=) var) vars) names
        let nfa = NFA.projection nfa indices_to_squash
        let vars = List.filter (fun x -> not <| List.exists ((=) x) names) vars
        nfa.ToDFA () |> NFA.quotientZero, vars
    | e -> failwithf $"Unsupported literal {e}."
