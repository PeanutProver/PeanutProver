namespace PeanutProver.Automata

open Ast.Common

type State = { Name: string; Id: int }

type Result =
    | Accept
    | Partial of bit list list
    | Fail of State seq

type DFA(startState, finalStates: _ seq, transitions) =
    let rec recognize input states =
        match input with
        | [] ->
            if states |> Seq.exists (fun state -> Seq.contains state finalStates) then
                Accept
            else
                Fail states
        | hd :: tl ->
            let find state =
                match Map.tryFind state transitions with
                | Some(wordMapping: Map<bit list, State seq>) -> Map.tryFind hd wordMapping
                | _ -> None
                |> (Option.toList)
                |> List.toSeq
                |> Seq.concat in

            states |> Seq.map (find) |> Seq.concat |> Seq.distinct |> recognize tl

    member this.StartState = startState
    member this.FinalStates = finalStates
    member this.Transitions = transitions

    member this.Recognize input =
        recognize input <| Seq.singleton startState

    member this.ToDot() =
        let nodes = ResizeArray<_>()
        let edges = ResizeArray<_>()

        for (fromState, letterInfo) in Map.toSeq transitions do
            let nodeColor = if fromState = startState then "green" else "white"

            let nodeShape =
                if Seq.contains fromState finalStates then
                    "doublecircle"
                else
                    "circle"

            nodes.Add(
                $"{fromState.Id} [fillcolor={nodeColor} shape={nodeShape} style=filled label=\"{fromState.Name}\"]"
            )

            for (letter, toStates) in Map.toSeq letterInfo do
                edges.Add($"{fromState.Id} -> %A{toStates |> Seq.distinct} [label=\"%A{letter}\"]")

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
            |> Seq.filter (not << (fun x -> Seq.contains x dfa.FinalStates))
            |> Set.ofSeq,
            dfa.Transitions
        )

    let intersection (dfa1: DFA) (dfa2: DFA) =
        let stateNumber2 = dfa2.Transitions.Count

        let makeNewState (state1, state2) =
            { Name = $"{state1.Name}_{state2.Name}"
              Id = state1.Id * stateNumber2 + state2.Id }

        let makeNewStates (fst, snd) =
            Seq.allPairs fst snd |> Seq.map makeNewState

        let startState = makeNewState (dfa1.StartState, dfa2.StartState)

        let transitions =
            Seq.allPairs dfa1.Transitions dfa2.Transitions
            |> Seq.map (fun (first, second) ->
                (makeNewState (first.Key, second.Key),
                 first.Value
                 |> Seq.map (fun node -> (node.Key, makeNewStates (node.Value, second.Value[node.Key])))
                 |> Map.ofSeq))
            |> Map.ofSeq

        let finalStates =
            Seq.allPairs dfa1.FinalStates dfa2.FinalStates
            |> Seq.map makeNewState
            |> Set.ofSeq

        DFA(startState, finalStates, transitions)

    let union (dfa1: DFA) (dfa2: DFA) =
        complement (intersection (complement dfa1) (complement dfa2))

    let removeManyAt ids ls =
        ls
        |> List.indexed
        |> List.filter (fun (idx, _) -> not <| List.exists ((=) idx) ids)
        |> List.map snd

    let projection (dfa: DFA) vars =
        dfa.Transitions
        |> Map.toSeq
        |> Seq.map (fun (key, value) ->
            value
            |> Map.toSeq
            |> Seq.fold
                (fun acc (bitV, states) ->
                    let newBit = removeManyAt vars bitV

                    Map.tryFind newBit acc
                    |> (function
                    | None -> Map.add newBit states acc
                    | Some accStates -> Map.add newBit (Seq.append accStates states) acc))
                Map.empty
            |> fun newMap -> key, newMap)
        |> fun x -> DFA(dfa.StartState, dfa.FinalStates, Map.ofSeq x)
