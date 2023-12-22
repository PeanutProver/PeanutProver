namespace PeanutProver.Automata

open Ast.Common

type State = { Name: string; Id: int }

module State =

    let nextId = ref 0 

    let get () = 
        let current = nextId.Value 
        nextId.Value <- nextId.Value + 1
        current 

    let createStaet () = {Name = ""; Id =  get ()}
    

type Result =
    | Accept
    | Fail of State seq

type NFA(startState, finalStates: _ seq, transitions) =
    let rec recognize input states =
        match input with
        | [] ->
            if states |> Seq.exists (fun state -> Seq.contains state finalStates) then
                Accept
            else
                Fail states
        | hd :: tl ->
            let find state =
                Map.tryFind state transitions
                |> Option.bind (Map.tryFind hd)
                |> Option.toList
                |> List.toSeq
                |> Seq.concat in

            states |> Seq.map (find) |> Seq.concat |> Seq.distinct |> recognize tl

    member this.StartState = startState
    member this.FinalStates = finalStates
    member this.Transitions = transitions
    member this.AllStates = 
        let sourceStates = Map.keys transitions |> Set.ofSeq 
        let destinationStates = transitions |> Map.values |> Seq.map Map.values |> Seq.concat |> Seq.concat |> Set.ofSeq 

        let startSet = Set.singleton startState 
        let finalState = Set.ofSeq finalStates 

        Seq.fold Set.union Set.empty [sourceStates; destinationStates; startSet; finalState ]

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
                for toState in toStates do
                    edges.Add($"{fromState.Id} -> %A{toState.Id} [label=\"%A{letter}\"]")

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

    member this.ToDFA () =
        let states = this.AllStates

        let powerSet =
             List.ofSeq states
             |> List.powerSet 
             |> Set.ofList 
             |> Seq.filter (not << List.isEmpty)
            |> Seq.distinct
            |> Seq.map Set.ofList 


        let newStateMap = 
            powerSet 
            |> Seq.map (fun set -> set, State.createStaet ())
            |> Map.ofSeq 

        let lookUpState s = Map.find s newStateMap  

        printfn "Source start states"
        printfn "%A" startState

        printfn "Source transitions"
        printfn "%A" transitions

        printfn "Source final states"
        printfn "%A" finalStates

        let map: (State * bit list * State seq) seq =
            transitions
            |> Map.map (fun _ v -> Map.toSeq v)
            |> Map.toSeq
            |> Seq.map (fun (st, tl) -> Seq.map (fun (bl, sts) -> st, bl, sts) tl)
            |> Seq.concat in

        let step sts =
            let is x = Seq.exists ((=) x) sts in

            map
            |> Seq.filter (fun (st, _, _) -> is st)
            |> Seq.groupBy (fun (_, bl, _) -> bl)
            |> Seq.map (fun (bl, s) ->
                 let tg = 
                    Seq.map (fun (_, _, tg) -> tg) s |> Seq.concat |> Seq.distinct 
                 in (bl, tg))
            |> fun x -> (sts, x)

        let transitions = 
            powerSet
            |> Seq.map step
            |> Seq.map (fun (source, blAndTarget) -> 
                let rawMap = blAndTarget |> Seq.map (fun (bl, states) -> (bl, states |> Set.ofSeq |> lookUpState|> Seq.singleton))
                let value = Map.ofSeq rawMap
                let key = lookUpState source
                
                (key, value))
            |> Map.ofSeq 

        let startState = Set.singleton startState |> lookUpState

        let finalStates = 
            let isFinal states = 
                Seq.exists (fun st -> Seq.exists ((=)st) finalStates) states 
        
            powerSet |> Seq.filter isFinal 
            |> Seq.map lookUpState

        printfn "Final start states"
        printfn "%A" startState

        printfn "Final transitions"
        printfn "%A" transitions

        printfn "Final final states"
        printfn "%A" finalStates

        NFA (startState, finalStates, transitions)

module NFA =
    let isDfa (fa: NFA) =
        fa.Transitions
        |> Map.forall (fun _ value ->
            value
            |> Map.values
            |> Seq.forall (fun transitions -> transitions |> Seq.length = 1))

    
    let complement (nfa: NFA) =
        NFA(
            nfa.StartState,
            nfa.Transitions
            |> Seq.map (fun node -> node.Key)
            |> Seq.filter (not << (fun x -> Seq.contains x nfa.FinalStates))
            |> Set.ofSeq,
            nfa.Transitions
        )

    let intersection (nfa1: NFA) (nfa2: NFA) =
        let stateNumber2 = nfa2.Transitions.Count

        let makeNewState (state1, state2) =
            { Name = $"{state1.Name}_{state2.Name}"
              Id = state1.Id * stateNumber2 + state2.Id }

        let makeNewStates (fst, snd) =
            Seq.allPairs fst snd |> Seq.map makeNewState

        let startState = makeNewState (nfa1.StartState, nfa2.StartState)

        let transitions =
            Seq.allPairs nfa1.Transitions nfa2.Transitions
            |> Seq.map (fun (first, second) ->
                (makeNewState (first.Key, second.Key),
                 first.Value
                 |> Seq.map (fun node -> (node.Key, makeNewStates (node.Value, second.Value[node.Key])))
                 |> Map.ofSeq))
            |> Map.ofSeq

        let finalStates =
            Seq.allPairs nfa1.FinalStates nfa2.FinalStates
            |> Seq.map makeNewState
            |> Set.ofSeq

        NFA(startState, finalStates, transitions)

    let union (nfa1: NFA) (nfa2: NFA) =
        let states1 = nfa1.Transitions |> Seq.map (fun node -> node.Key)
        let states2 = nfa2.Transitions |> Seq.map (fun node -> node.Key)
        let stateNumber2 = nfa2.Transitions.Count

        let makeNewState (state1, state2) =
            { Name = $"{state1.Name}_{state2.Name}"
              Id = state1.Id * stateNumber2 + state2.Id }

        let makeNewStates (fst, snd) =
            Seq.allPairs fst snd |> Seq.map makeNewState

        let startState = makeNewState (nfa1.StartState, nfa2.StartState)

        let transitions =
            Seq.allPairs nfa1.Transitions nfa2.Transitions
            |> Seq.map (fun (first, second) ->
                (makeNewState (first.Key, second.Key),
                 first.Value
                 |> Seq.map (fun node -> (node.Key, makeNewStates (node.Value, second.Value[node.Key])))
                 |> Map.ofSeq))
            |> Map.ofSeq

        let finalStates =

            [ Seq.allPairs states1 nfa2.FinalStates; Seq.allPairs nfa1.FinalStates states2 ]
            |> Seq.concat
            |> Seq.map makeNewState
            |> Set.ofSeq

        NFA(startState, finalStates, transitions)

    let removeManyAt ids ls =
        ls
        |> List.indexed
        |> List.filter (fun (idx, _) -> not <| List.exists ((=) idx) ids)
        |> List.map snd

    let projection (nfa: NFA) vars =
        nfa.Transitions
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
        |> fun x -> NFA(nfa.StartState, nfa.FinalStates, Map.ofSeq x)
