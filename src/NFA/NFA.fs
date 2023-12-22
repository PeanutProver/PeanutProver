namespace PeanutProver.Automata

open System.Collections.Generic
open Ast.Common
open Microsoft.FSharp.Collections

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

    member this.CountOfBits =
        transitions |> Map.find startState |> Map.keys |> Seq.head |> List.length

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
        let nodeColor state =
            if state = startState then "green" else "white"

        let nodeShape state =
            if Seq.contains state finalStates then
                "doublecircle"
            else
                "circle"

        let nodes = ResizeArray<string>()
        let edges = ResizeArray<string>()

        transitions
        |> Map.iter (fun fromState letterInfo ->
            nodes.Add(
                $"{fromState.Id} [fillcolor={nodeColor fromState} shape={nodeShape fromState} style=filled label=\"{fromState.Name}\"]"
            )

            letterInfo
            |> Map.iter (fun letter toStates ->
                toStates
                |> Seq.iter (fun toState -> (edges.Add($"{fromState.Id} -> %A{toState.Id} [label=\"%A{letter}\"]")))))

        nodes.Sort()
        edges.Sort()

        seq {
            "digraph g {"
            "rankdir=\"LR\""
            yield! nodes
            yield! edges
            "}"
        }
        |> Seq.distinct
        |> String.concat "\n"

    member this.ToDot(filePath: string) =
        let dot = this.ToDot()
        System.IO.File.WriteAllText(filePath, dot)

    member this.Alphabet =
        transitions |> Map.values |> Seq.map Map.keys |> Seq.concat |> Seq.distinct

    member this.Reachable(state: State) =
        let rec dfs (state: State) marked =
            if Set.contains state marked then
                marked
            else
                let marked = Set.add state marked

                transitions
                |> Map.find state
                |> Map.values
                |> Seq.concat
                |> Set.ofSeq
                |> Set.fold (fun acc x -> acc |> Set.union (dfs x marked)) (Set.singleton state)

        dfs state Set.empty

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

    let minimization (nfa: NFA) =
        if isDfa nfa then
            let alphabet = nfa.Alphabet
            let allTransitions = nfa.Transitions
            let allStates = nfa.AllStates |> Set
            let finalStates = nfa.FinalStates |> Set

            let unionStatesToOne (states: Set<State>) =
                let name, id =
                    states |> Seq.fold (fun acc x -> (fst acc + x.Name, snd acc + x.Id)) ("", 0)

                { Name = name; Id = id }

            let reverseEdgesMap =
                alphabet
                |> Seq.map (fun symbol ->
                    let map =
                        allTransitions
                        |> Map.map (fun stateFrom trans ->
                            trans
                            |> Map.filter (fun vector statesTo -> vector = symbol)
                            |> Map.values
                            |> Seq.concat
                            |> Set.ofSeq)

                    symbol, map)
                |> Map.ofSeq

            let reverseEdges (bitList: bit list) (state: State) =
                reverseEdgesMap
                |> Map.find bitList
                |> Map.filter (fun _ -> Set.contains state)
                |> Map.keys
                |> Set

            let splitters = Queue<Set<State> * bit list>()

            let equivalenceClassesStart =
                Set.ofList [ finalStates; Set.difference allStates finalStates ]

            for cl in equivalenceClassesStart do
                alphabet |> Seq.iter (fun symbol -> splitters.Enqueue(cl, symbol))

            let rec iterateOverQueue (splitters: Queue<Set<State> * bit list>) (equivalenceClasses: Set<Set<State>>) =
                if splitters.Count = 0 then
                    equivalenceClasses
                else
                    let (splitter: Set<State>, symbol: bit list) = splitters.Dequeue()

                    equivalenceClasses
                    |> Seq.fold
                        (fun acc eqClass ->
                            let tempEqClasses = acc |> Set.filter (eqClass.Equals >> not)

                            let r1 =
                                splitter
                                |> Set.map (reverseEdges symbol)
                                |> Set.unionMany
                                |> Set.intersect eqClass

                            let r2 = Set.difference eqClass r1

                            if r1.Count <> 0 && r2.Count <> 0 then
                                if r1.Count < r2.Count then
                                    alphabet |> Seq.iter (fun symbol' -> splitters.Enqueue(r1, symbol'))
                                else
                                    alphabet |> Seq.iter (fun symbol' -> splitters.Enqueue(r2, symbol'))

                                tempEqClasses |> Set.add r1 |> Set.add r2
                            else
                                acc)
                        (equivalenceClasses)
                    |> iterateOverQueue splitters

            let equivalenceClasses = iterateOverQueue splitters equivalenceClassesStart

            let findEqClassByState state =
                equivalenceClasses |> Set.filter (Set.contains state) |> Set.toList |> List.head

            let eqClassState =
                equivalenceClasses |> Seq.map (fun cl -> cl, unionStatesToOne cl) |> Map.ofSeq

            let unionStatesWithTrans
                (state1: State, trans1: (bit list * State seq) seq)
                (_, trans2: (bit list * State seq) seq)
                =
                let commonTr = Seq.append trans2 trans1 |> Seq.distinct
                state1, commonTr

            let newFinalStates =
                nfa.FinalStates
                |> Seq.map (fun state -> eqClassState |> Map.find (state |> findEqClassByState))

            let newStartState = eqClassState |> Map.find (nfa.StartState |> findEqClassByState)


            let newTransitionsTemp =
                allTransitions
                |> Map.toSeq
                |> Seq.map (fun (stateFrom, tr) ->
                    eqClassState |> Map.find (findEqClassByState stateFrom),
                    tr
                    |> Map.toSeq
                    |> Seq.map (fun (symbol, statesTo) ->
                        symbol,
                        statesTo
                        |> Seq.map (fun stateTo -> eqClassState |> Map.find (findEqClassByState stateTo))))

            let newTransitions =
                newTransitionsTemp
                |> Seq.map fst
                |> Seq.distinct
                |> Seq.map (fun state ->
                    newTransitionsTemp
                    |> Seq.filter (fun (s, _) -> state = s)
                    |> Seq.reduce unionStatesWithTrans)
                |> Seq.map (fun (s, t) -> s, Seq.toList t |> Map.ofList)
                |> Map.ofSeq


            let resultNfa = NFA(newStartState, newFinalStates, newTransitions)
            let reachable = resultNfa.StartState |> resultNfa.Reachable

            let transitionsFromStart =
                Map.filter (fun stateFrom _ -> Set.contains stateFrom reachable) newTransitions

            NFA(newStartState, newFinalStates, transitionsFromStart)
        else
            nfa

    let quotientZero (nfa: NFA) =
        let zeroesList = List.replicate nfa.CountOfBits Zero
        let allStates = nfa.AllStates
        let countOfStates = allStates |> Set.count

        let step states =
            states
            |> Set.fold
                (fun acc state ->
                    nfa.Transitions
                    |> Map.find state
                    |> Map.find zeroesList
                    |> Set.ofSeq
                    |> Set.union acc)
                Set.empty

        let accept (state: State) =
            let counter = 0

            let rec anotherStep counter states =
                if (states |> Set.intersect (Set.ofSeq nfa.FinalStates) |> Set.count) = 0 then
                    if counter > countOfStates then
                        false
                    else
                        anotherStep (counter + 1) (step states)
                else
                    true

            anotherStep 0 (Set.ofList [ state ])

        let newFinalSates =
            allStates
            |> Set.fold
                (fun acc state -> if accept state then acc |> Set.add state else acc)
                (Set.ofSeq nfa.FinalStates)

        NFA(nfa.StartState, newFinalSates, nfa.Transitions)
