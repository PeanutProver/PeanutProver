namespace PeanutProver.Automata

open System.Collections.Generic

open Ast.Common

type State<'a when 'a: equality>(id: 'a, isStart: bool, isFinal: bool) =
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


type Result<'a when 'a: equality> =
    | Accept
    | Partial of bit list list
    | Fail of State<'a>

[<Struct>]
type Configuration<'a when 'a: equality> =
    val CurrentState: State<'a>
    val RestOfInput: bit list list

    new(state, rest) =
        { CurrentState = state
          RestOfInput = rest }

type DFA<'a when 'a: equality>(startState: State<'a>) =
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

    let allStates, alphabet =
        let allStates = HashSet<_>()
        let alphabet = HashSet<_>()

        let dfs (state: State<_>) =
            let visited = HashSet<_>()
            let toProcess = Stack<_> [ state ]

            while toProcess.Count > 0 do
                let currentState = toProcess.Pop()
                let added = allStates.Add currentState
                // assert added
                let added = visited.Add currentState
                // assert added
                for (char, targetState) in currentState.GetAllTransitions() do
                    alphabet.Add char |> ignore

                    if visited.Contains targetState |> not then
                        toProcess.Push targetState

        dfs startState
        allStates, alphabet

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

    member this.AllStates = allStates
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


    let rec update_trans trans in_head =
        if in_head then
            [ Zero :: trans; One :: trans ]
        else
            [ trans @ [ Zero ]; trans @ [ One ] ]

    let add_transitions_range (from_state: State<_>) (trans_list: bit list list) (to_state: State<_>) =
        List.iter (fun x -> from_state.AddTransition x to_state) trans_list

    let findByOld seqPairs (old: State<'a>) =
        Seq.pick
            (fun (oldS: State<'a>, newS) ->
                match oldS with
                | oldS when oldS.Id = old.Id -> Some newS
                | _ -> None)
            seqPairs

    let rec inflateTransitions (n: int) (inHead: bool) (dfa: DFA<_>) =
        if n <= 0 then
            dfa
        else
            let allStatesOld = dfa.AllStates

            let allStatesNew = HashSet()

            for state in allStatesOld do
                allStatesNew.Add(State<_>(state.Id + "'", state.IsStart, state.IsFinal))
                |> ignore

            let allStatesZipped = Seq.zip allStatesOld allStatesNew

            Seq.iter
                (fun (oldState: State<_>, newState: State<_>) ->
                    oldState.GetAllTransitions()
                    |> List.iter (fun (tr, st) ->
                        add_transitions_range newState (update_trans tr inHead) (findByOld allStatesZipped st)))
                allStatesZipped

            let startStateNew =
                allStatesNew
                |> Seq.pick (fun x ->
                    match x with
                    | x when x.IsStart -> Some x
                    | _ -> None)

            inflateTransitions (n - 1) inHead (DFA startStateNew)

    let permDfa (dfa: DFA<_>) perm =

        let permute ((oldState: State<_>, transitions: (bit list * State<_>) list)) =
            oldState,
            List.map
                (fun (tr: bit list, st: State<_>) -> (List.permute (fun t -> (List.item t perm)) tr, st))
                transitions


        let oldStates = dfa.AllStates
        let newStates = HashSet()

        for oldState in oldStates do
            newStates.Add(State<_>(oldState.Id, oldState.IsStart, oldState.IsFinal))
            |> ignore

        let zippedStates = Seq.zip oldStates newStates

        let permutedTransitions =
            Seq.map (fun state -> permute (state, state.GetAllTransitions())) oldStates

        permutedTransitions
        |> Seq.iter (fun (fromOldState, transitions) ->
            let fromNewState = (findByOld zippedStates fromOldState)

            List.iter
                (fun (trans, toOldState) ->
                    let toNewState = (findByOld zippedStates toOldState)
                    fromNewState.AddTransition trans toNewState)
                transitions)

        let startStateNew =
            newStates
            |> Seq.pick (fun x ->
                match x with
                | x when x.IsStart -> Some x
                | _ -> None)

        DFA startStateNew
