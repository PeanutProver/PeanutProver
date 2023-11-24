module PeanutProver.DFA.PredefinedAutomata

open PeanutProver.Automata
open Ast.Common

let set = Seq.singleton

let dfa_eq =
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

    DFA(startState, finalStates, transitions)

let dfa_less =
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

    DFA(startState, finalStates, transitions)

let dfa_sum =
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

    DFA(noCarry, finalStates, transitions)

let dfa_not_eq = DFA.complement dfa_eq

let dfa_less_eq = DFA.union dfa_eq dfa_less

let dfa_bitwise_minimum =
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

    DFA(start, finalStates, transitions)
