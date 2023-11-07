﻿namespace PeanutProver.Automata

open System.Collections.Generic

open Ast.Common

type State<'a>(id: 'a, isStart: bool, isFinal: bool) =
    let transitions = Dictionary<bit list, State<'a>>()

    member this.AddTransition tupleOfChars state =
        if transitions.ContainsKey(tupleOfChars) then
            failwithf $"Transition for symbol {tupleOfChars} already exists!"

        transitions.Add(tupleOfChars, state)

    member this.IsStart = isStart
    member this.IsFinal = isFinal
    member this.Id = id

    member this.GoesTo char =
        let exists, v = transitions.TryGetValue char
        if exists then Some v else None

    override this.ToString() = string this.Id

    member this.GetAllTransitions() =
        [ for kvp in transitions -> kvp.Key, kvp.Value ]


type Result<'a> =
    | Accept
    | Partial of bit list list
    | Fail of State<'a>

[<Struct>]
type Configuration<'a> =
    val CurrentState: State<'a>
    val RestOfInput: bit list list

    new(state, rest) =
        { CurrentState = state
          RestOfInput = rest }

type DFA<'a>(startState: State<'a>) =
    let rec step (configuration: Configuration<'a>) =
        match configuration.RestOfInput with
        | [] ->
            if configuration.CurrentState.IsFinal then
                Accept
            else
                Fail configuration.CurrentState

        | hd :: tl ->
            match configuration.CurrentState.GoesTo hd with
            | None -> Partial configuration.RestOfInput
            | Some nextState -> step (Configuration(nextState, tl))

    member this.StartState = startState
    member this.Recognize str = step (Configuration(startState, str))

    member this.ToDot() =
        let nodes = ResizeArray<_>()
        let edges = ResizeArray<_>()
        let mutable firstFreeNodeId = 0

        let dfs (state: State<_>) =
            let toProcess = Stack<State<_> * (int -> unit)>[(state, (fun _ -> ()))]
            let copyOfState = Dictionary<State<_>, int>()

            while toProcess.Count > 0 do
                let state, addTransitions = toProcess.Pop()
                let exists, newState = copyOfState.TryGetValue state

                if not exists then
                    let curNodeId = firstFreeNodeId
                    firstFreeNodeId <- firstFreeNodeId + 1
                    let nodeColor = if state.IsStart then "green" else "white"
                    let nodeShape = if state.IsFinal then "doublecircle" else "circle"

                    nodes.Add(
                        $"{curNodeId} [fillcolor={nodeColor} shape={nodeShape} style=filled label=\"{state.Id}\"]"
                    )

                    addTransitions curNodeId

                    for (char, targetState) in state.GetAllTransitions() do
                        toProcess.Push(
                            targetState,
                            (fun newTargetStateId -> edges.Add($"{curNodeId} -> {newTargetStateId} [label=\"{char}\"]"))
                        )

                    copyOfState.Add(state, curNodeId)
                else
                    addTransitions newState

        dfs this.StartState

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
// member this.AllStates = allStates
// member this.FinalStates = allStates |> Seq.filter (fun x -> x.IsFinal) |> HashSet
// member this.Alphabet = alphabet

module DFA =
    let complement (dfa: DFA<_>) =
        let dfs (state: State<_>) =
            let mutable newStartState = Unchecked.defaultof<_>
            let toProcess = Stack<State<_> * (State<_> -> unit)>[(state, (fun _ -> ()))]
            let copyOfState = Dictionary<_, _>()

            while toProcess.Count > 0 do
                let state, addTransitions = toProcess.Pop()
                let exists, newState = copyOfState.TryGetValue state

                if not exists then
                    let newState = State<_>(state.Id, state.IsStart, not state.IsFinal)

                    if newState.IsStart then
                        newStartState <- newState

                    addTransitions newState

                    for (char, targetState) in state.GetAllTransitions() do
                        toProcess.Push(targetState, (fun newTargetState -> newState.AddTransition char newTargetState))

                    copyOfState.Add(state, newState)
                else
                    addTransitions newState

            newStartState

        let newStartState = dfs dfa.StartState
        DFA(newStartState)

    let intersection (dfa1: DFA<_>) (dfa2: DFA<_>) =
        let dfs (state1: State<_>) (state2: State<_>) =
            let mutable newStartState = Unchecked.defaultof<_>

            let toProcess =
                Stack<State<_> * State<_> * (State<_> -> unit)>[(state1, state2, (fun _ -> ()))]

            let copyOfState = Dictionary<_, _>()

            while toProcess.Count > 0 do
                let state1, state2, addTransitions = toProcess.Pop()
                let exists, newState = copyOfState.TryGetValue((state1, state2))

                if not exists then
                    let newState =
                        State<_>(
                            $"{state1.Id}_{state2.Id}",
                            state1.IsStart && state2.IsStart,
                            state1.IsFinal && state2.IsFinal
                        )

                    if newState.IsStart then
                        newStartState <- newState

                    addTransitions newState

                    for (char1, targetState1) in state1.GetAllTransitions() do
                        for (char2, targetState2) in state2.GetAllTransitions() do
                            if char1 = char2 then
                                toProcess.Push(
                                    targetState1,
                                    targetState2,
                                    (fun newTargetState -> newState.AddTransition char1 newTargetState)
                                )

                    copyOfState.Add((state1, state2), newState)
                else
                    addTransitions newState

            newStartState

        let newStartState = dfs dfa1.StartState dfa2.StartState
        DFA(newStartState)

    let union dfa1 dfa2 =
        complement (intersection (complement dfa1) (complement dfa2))
