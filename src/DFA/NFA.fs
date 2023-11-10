namespace PeanutProver.Automata.NFA

open System.Collections.Generic

type State<'stateName, 'char when 'char: equality>(id: 'stateName, isStart: bool, isFinal: bool) =
    let transitions = Dictionary<'char, HashSet<State<'stateName, 'char>>>()

    member this.AddTransition tupleOfChars state =
        let exists, v = transitions.TryGetValue(tupleOfChars)

        if exists then
            v.Add state |> ignore
        else
            transitions.Add(tupleOfChars, HashSet<_>[state])

    member this.IsStart = isStart
    member this.IsFinal = isFinal
    member this.Id = id

    member this.GoesTo char =
        let exists, v = transitions.TryGetValue char
        if exists then Some v else None

    override this.ToString() = string this.Id

    member this.GetAllTransitions() =
        [ for kvp in transitions -> kvp.Key, kvp.Value ]


type Result =
    | Accept
    | Fail

[<Struct>]
type Configuration<'a, 'char when 'char: equality> =
    val CurrentState: State<'a, 'char>
    val RestOfInput: list<'char>

    new(state, rest) =
        { CurrentState = state
          RestOfInput = rest }

type NFA<'a, 'char when 'char: equality>(startState: State<'a, 'char>) =

    let step (configuration: Configuration<'a, 'char>) =
        let configToProcess = Stack<Configuration<'a, 'char>> [ configuration ]
        let mutable accepted = false

        while (not accepted) && configToProcess.Count > 0 do
            let configuration = configToProcess.Pop()

            match configuration.RestOfInput with
            | [] -> accepted <- configuration.CurrentState.IsFinal

            | hd :: tl ->
                match configuration.CurrentState.GoesTo hd with
                | None -> ()
                | Some nextStates ->
                    for nextState in nextStates do
                        configToProcess.Push(Configuration(nextState, tl))

        if accepted then Accept else Fail

    member this.StartState = startState
    member this.Recognize str = step (Configuration(startState, str))

    member this.ToDot() =
        let nodes = ResizeArray<_>()
        let edges = ResizeArray<_>()
        let mutable firstFreeNodeId = 0

        let dfs (state: State<_, _>) =
            let toProcess = Stack<State<_, _> * (int -> unit)>[(state, (fun _ -> ()))]
            let copyOfState = Dictionary<State<_, _>, int>()

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

                    for (char, targetStates) in state.GetAllTransitions() do
                        for targetState in targetStates do
                            toProcess.Push(
                                targetState,
                                (fun newTargetStateId ->
                                    edges.Add($"{curNodeId} -> {newTargetStateId} [label=\"{char}\"]"))
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
