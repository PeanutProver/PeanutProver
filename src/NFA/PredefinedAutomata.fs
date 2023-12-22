module PeanutProver.NFA.PredefinedAutomata

open PeanutProver.Automata
open Ast.Common

let set = Seq.singleton

let nfa_eq =
    let startState = { Name = "eq:Start"; Id = 0 }
    let failedState = { Name = "eq:Failed"; Id = 1 }
    let finalStates = set startState

    let transitions =
        Map
            [ startState,
              Map
                  [ [ Zero; Zero ], set startState
                    [ One; One ], set startState
                    [ One; Zero ], set failedState
                    [ Zero; One ], set failedState ]
              failedState,
              Map
                  [ [ Zero; Zero ], set failedState
                    [ One; One ], set failedState
                    [ One; Zero ], set failedState
                    [ Zero; One ], set failedState ] ]

    NFA(startState, finalStates, transitions)

let nfa_less =
    let startState = { Name = "less:Start"; Id = 0 }
    let lessState = { Name = "less:Less"; Id = 1 }
    let finalStates = set lessState

    let transitions =
        Map
            [ startState,
              Map
                  [ [ Zero; Zero ], set startState
                    [ One; One ], set startState
                    [ One; Zero ], set startState
                    [ Zero; One ], set lessState ]
              lessState,
              Map
                  [ [ Zero; Zero ], set lessState
                    [ One; One ], set lessState
                    [ One; Zero ], set startState
                    [ Zero; One ], set lessState ] ]

    NFA(startState, finalStates, transitions)

type TreeNode =
    | Node of TreeNode * TreeNode
    | Leaf of int seq array

let nfa_sum =
    let state_names = [| "noCarry"; "carry"; "fail" |]
    let no_carry = 0
    let carry = 1
    let fail = 2
    let startState = no_carry
    let finalStates = set no_carry

    let transitions =
        Node(
            Node(
                Node(
                    Leaf([| set no_carry; set fail; set fail |] (*0;0;0*) ),
                    Leaf([| set fail; set no_carry; set fail |] (*0;0;1*) )
                ),
                Node(
                    Leaf([| set fail; set carry; set fail |] (*0;1;0*) ),
                    Leaf([| set no_carry; set fail; set fail |] (*0;1;1*) )
                )
            ),
            Node(
                Node(
                    Leaf([| set fail; set carry; set fail |] (*1;0;0*) ),
                    Leaf([| set no_carry; set fail; set fail |] (*1;0;1*) )
                ),
                Node(
                    Leaf([| set carry; set fail; set fail |] (*1;1;0*) ),
                    Leaf([| set fail; set carry; set fail |] (*1;1;1*) )
                )
            )
        )

    let noCarry = { Name = "sum:noCarry"; Id = 0 }
    let carry = { Name = "sum:carry"; Id = 1 }
    let fail = { Name = "sum:fail"; Id = 2 }
    let finalStates = set noCarry

    let transitions =
        Map
            [ noCarry,
              Map
                  [ [ Zero; Zero; Zero ], set noCarry
                    [ Zero; Zero; One ], set fail
                    [ Zero; One; Zero ], set fail
                    [ Zero; One; One ], set noCarry
                    [ One; Zero; Zero ], set fail
                    [ One; Zero; One ], set noCarry
                    [ One; One; Zero ], set carry
                    [ One; One; One ], set fail ]
              carry,
              Map
                  [ [ Zero; Zero; Zero ], set fail
                    [ Zero; Zero; One ], set noCarry
                    [ Zero; One; Zero ], set carry
                    [ Zero; One; One ], set fail
                    [ One; Zero; Zero ], set carry
                    [ One; Zero; One ], set fail
                    [ One; One; Zero ], set fail
                    [ One; One; One ], set carry ]
              fail,
              Map
                  [ [ Zero; Zero; Zero ], set fail
                    [ Zero; Zero; One ], set fail
                    [ Zero; One; Zero ], set fail
                    [ Zero; One; One ], set fail
                    [ One; Zero; Zero ], set fail
                    [ One; Zero; One ], set fail
                    [ One; One; Zero ], set fail
                    [ One; One; One ], set fail ] ]

    NFA(noCarry, finalStates, transitions)

let nfa_not_eq = NFA.complement nfa_eq

let nfa_less_eq = NFA.union nfa_eq nfa_less

let nfa_bitwise_minimum =
    let start = { Name = "bm:start"; Id = 0 }
    let fail = { Name = "bm:fail"; Id = 1 }
    let finalStates = set start

    let transitions =
        Map
            [ start,
              Map[[ Zero; Zero; Zero ], set start
                  [ Zero; Zero; One ], set fail
                  [ Zero; One; Zero ], set start
                  [ Zero; One; One ], set fail
                  [ One; Zero; Zero ], set start
                  [ One; Zero; One ], set fail
                  [ One; One; Zero ], set fail
                  [ One; One; One ], set start]
              fail,
              Map[[ Zero; Zero; Zero ], set fail
                  [ Zero; Zero; One ], set fail
                  [ Zero; One; Zero ], set fail
                  [ Zero; One; One ], set fail
                  [ One; Zero; Zero ], set fail
                  [ One; Zero; One ], set fail
                  [ One; One; Zero ], set fail
                  [ One; One; One ], set fail] ]

    NFA(start, finalStates, transitions)

let fa_constant_eq (constant: int) : NFA =
    let automatonName = $"eq_const{constant}"

    let makeStateName obj = $"{automatonName}:{obj}"

    let makeState i =
        { Name = makeStateName $"bit{i}"
          Id = i }

    let constantBits = constant |> intToBits |> Seq.rev
    let constBitNum = Seq.length constantBits

    let allBitsRead = makeState constBitNum

    let fail =
        { Name = makeStateName "fail"
          Id = constBitNum + 1 }

    let transitions =
        constantBits
        |> Seq.mapi (fun index bit ->
            (makeState index, Map [ [ revBit bit ], set fail; [ bit ], set (makeState (index + 1)) ]))
        |> Map.ofSeq
        |> Map.add fail (Map [ [ Zero ], set fail; [ One ], set fail ])
        |> Map.add allBitsRead (Map [ [ Zero ], set allBitsRead; [ One ], set fail ])

    NFA(makeState 0, set allBitsRead, transitions)
