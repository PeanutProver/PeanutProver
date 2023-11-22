module PeanutProver.DFA.PredefinedAutomata

open PeanutProver.Automata
open Ast.Common

let dfa_eq =
    let startState = { Name = "eq:Start"; Id = 0 }
    let failedState = { Name = "eq:Failed"; Id = 1 }
    let finalStates = set [ startState ]

    let transitions =
        Map
            [ startState,
              Map
                  [ [ Zero; Zero ], startState
                    [ One; One ], startState
                    [ One; Zero ], failedState
                    [ Zero; One ], failedState ]
              failedState,
              Map
                  [ [ Zero; Zero ], failedState
                    [ One; One ], failedState
                    [ One; Zero ], failedState
                    [ Zero; One ], failedState ] ]

    DFA(startState, finalStates, transitions)

let dfa_less =
    let startState = { Name = "less:Start"; Id = 0 }
    let lessState = { Name = "less:Less"; Id = 1 }
    let finalStates = set [ lessState ]

    let transitions =
        Map
            [ startState,
              Map
                  [ [ Zero; Zero ], startState
                    [ One; One ], startState
                    [ One; Zero ], startState
                    [ Zero; One ], lessState ]
              lessState,
              Map
                  [ [ Zero; Zero ], lessState
                    [ One; One ], lessState
                    [ One; Zero ], startState
                    [ Zero; One ], lessState ] ]

    DFA(startState, finalStates, transitions)

let dfa_sum =
    let noCarry = { Name = "sum:noCarry"; Id = 0 }
    let carry = { Name = "sum:carry"; Id = 1 }
    let fail = { Name = "sum:fail"; Id = 2 }
    let finalStates = set [ noCarry ]

    let transitions =
        Map
            [ noCarry,
              Map
                  [ [ Zero; Zero; Zero ], noCarry
                    [ Zero; Zero; One ], fail
                    [ Zero; One; Zero ], fail
                    [ Zero; One; One ], noCarry
                    [ One; Zero; Zero ], fail
                    [ One; Zero; One ], noCarry
                    [ One; One; Zero ], carry
                    [ One; One; One ], fail ]
              carry,
              Map
                  [ [ Zero; Zero; Zero ], fail
                    [ Zero; Zero; One ], noCarry
                    [ Zero; One; Zero ], carry
                    [ Zero; One; One ], fail
                    [ One; Zero; Zero ], carry
                    [ One; Zero; One ], fail
                    [ One; One; Zero ], fail
                    [ One; One; One ], carry ]
              fail,
              Map
                  [ [ Zero; Zero; Zero ], fail
                    [ Zero; Zero; One ], fail
                    [ Zero; One; Zero ], fail
                    [ Zero; One; One ], fail
                    [ One; Zero; Zero ], fail
                    [ One; Zero; One ], fail
                    [ One; One; Zero ], fail
                    [ One; One; One ], fail ] ]

    DFA(noCarry, finalStates, transitions)

let dfa_not_eq = DFA.complement dfa_eq

let dfa_less_eq = DFA.union dfa_eq dfa_less

let dfa_bitwise_minimum =
    let start = { Name = "bm:start"; Id = 0 }
    let fail = { Name = "bm:fail"; Id = 1 }
    let finalStates = set [ start ]

    let transitions =
        Map
            [ start,
              Map[[ Zero; Zero; Zero ], start
                  [ Zero; Zero; One ], fail
                  [ Zero; One; Zero ], start
                  [ Zero; One; One ], fail
                  [ One; Zero; Zero ], start
                  [ One; Zero; One ], fail
                  [ One; One; Zero ], fail
                  [ One; One; One ], start]
              fail,
              Map[[ Zero; Zero; Zero ], fail
                  [ Zero; Zero; One ], fail
                  [ Zero; One; Zero ], fail
                  [ Zero; One; One ], fail
                  [ One; Zero; Zero ], fail
                  [ One; Zero; One ], fail
                  [ One; One; Zero ], fail
                  [ One; One; One ], fail] ]

    DFA(start, finalStates, transitions)
