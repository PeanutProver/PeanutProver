namespace PeanutProver.Automata

open Ast.Common

type State = { Name: string; Id: int }

type Result =
    | Accept
    | Partial of bit list list
    | Fail of State

type DFA(startState, finalStates, transitions) =
    let rec recognize input state =
        match input with
        | [] ->
            if Set.contains state finalStates then
                Accept
            else
                Fail state
        | hd :: tl ->
            match Map.tryFind state transitions with
            | Some wordMapping ->
                match Map.tryFind hd wordMapping with
                | Some nextState -> recognize tl nextState
                | _ -> Partial input
            | _ -> Partial input

    member this.StartState = startState
    member this.FinalStates = finalStates
    member this.Transitions = transitions
    member this.Recognize input = recognize input startState

    member this.ToDot() =
        let nodes = ResizeArray<_>()
        let edges = ResizeArray<_>()

        for (fromState, letterInfo) in Map.toSeq transitions do
            let nodeColor = if fromState = startState then "green" else "white"

            let nodeShape =
                if Set.contains fromState finalStates then
                    "doublecircle"
                else
                    "circle"

            nodes.Add(
                $"{fromState.Id} [fillcolor={nodeColor} shape={nodeShape} style=filled label=\"{fromState.Name}\"]"
            )

            for (letter, toState) in Map.toSeq letterInfo do
                edges.Add($"{fromState.Id} -> {toState.Id} [label=\"%A{letter}\"]")

        seq {
            "digraph g {"
            yield! nodes
            yield! edges
            "}"
        }
        |> String.concat "\n"

    member this.ToDot(filePath: string) =
        let dot = this.ToDot()
        System.IO.File.WriteAllText(filePath, dot)

module DFA =
    let complement (dfa: DFA) =
        DFA(
            dfa.StartState,
            dfa.Transitions
            |> Seq.map (fun node -> node.Key)
            |> Seq.filter (not << dfa.FinalStates.Contains)
            |> Set.ofSeq,
            dfa.Transitions
        )

    let intersection (dfa1: DFA) (dfa2: DFA) =
        let stateNumber2 = dfa2.Transitions.Count

        let makeNewState (state1, state2) =
            { Name = $"{state1.Name}_{state2.Name}"
              Id = state1.Id * stateNumber2 + state2.Id }

        let startState = makeNewState (dfa1.StartState, dfa2.StartState)

        let transitions =
            Seq.allPairs dfa1.Transitions dfa2.Transitions
            |> Seq.map (fun (first, second) ->
                (makeNewState (first.Key, second.Key),
                 first.Value
                 |> Seq.map (fun node -> (node.Key, makeNewState (node.Value, second.Value[node.Key])))
                 |> Map.ofSeq))
            |> Map.ofSeq

        let finalStates =
            Seq.allPairs dfa1.FinalStates dfa2.FinalStates
            |> Seq.map makeNewState
            |> Set.ofSeq

        DFA(startState, finalStates, transitions)

    let union (dfa1: DFA) (dfa2: DFA) =
        complement (intersection (complement dfa1) (complement dfa2))
