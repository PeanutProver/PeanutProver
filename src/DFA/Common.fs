module PeanutProver.DFA.Common

open Ast.Common
open PeanutProver.Automata

let renumber_transitions (old_transitions: Map<_, Map<_, _>>) total_variables automation_indices =
    let rec selfProduct sequence number =
        if number = 0 then
            Seq.singleton []
        else
            seq {
                for el in sequence do
                    for tail in (selfProduct sequence (number - 1)) do
                        yield (el :: tail)
            }

    (Map.toSeq old_transitions)
    |> Seq.map (fun (state, old_letter_transitions) ->
        state,
        selfProduct [ Zero; One ] total_variables
        |> Seq.map (fun new_values ->
            new_values, old_letter_transitions[automation_indices |> List.map (fun i -> new_values[i])])
        |> Map.ofSeq)
    |> Map.ofSeq
