module PeanutProver.DFA.PredefinedAutomata

open PeanutProver.Automata

let dfa_eq =
    let startState = State<_, _>("eq:Start", true, true)
    let q_1 = State<_, _>("eq:q_1", false, false)
    startState.AddTransition (0, 0) startState
    startState.AddTransition (1, 1) startState
    startState.AddTransition (1, 0) q_1
    startState.AddTransition (0, 1) q_1
    q_1.AddTransition (0, 0) q_1
    q_1.AddTransition (1, 0) q_1
    q_1.AddTransition (0, 1) q_1
    q_1.AddTransition (1, 1) q_1
    DFA startState

let dfa_less =
    let startState = State<_, _>("less:Start", true, false)
    let q_1 = State<_, _>("less:q_1", false, true)
    startState.AddTransition (0, 1) q_1
    startState.AddTransition (1, 1) startState
    startState.AddTransition (1, 0) startState
    startState.AddTransition (0, 0) startState
    q_1.AddTransition (1, 0) startState
    q_1.AddTransition (0, 1) q_1
    q_1.AddTransition (1, 1) q_1
    q_1.AddTransition (0, 0) q_1
    DFA startState

let dfa_sum =
    let noCarry = State<_, _>("sum:noCarry", true, true)
    let carry = State<_, _>("sum:carry", false, false)
    let q_1 = State<_, _>("sum:q_1", false, false)
    noCarry.AddTransition (0, 0, 0) noCarry
    noCarry.AddTransition (0, 1, 1) noCarry
    noCarry.AddTransition (1, 0, 1) noCarry
    noCarry.AddTransition (1, 1, 0) carry
    noCarry.AddTransition (0, 1, 0) q_1
    noCarry.AddTransition (1, 0, 0) q_1
    noCarry.AddTransition (0, 0, 1) q_1
    noCarry.AddTransition (1, 1, 1) q_1
    q_1.AddTransition (0, 0, 0) q_1
    q_1.AddTransition (0, 0, 1) q_1
    q_1.AddTransition (0, 1, 0) q_1
    q_1.AddTransition (0, 1, 1) q_1
    q_1.AddTransition (1, 0, 0) q_1
    q_1.AddTransition (1, 0, 1) q_1
    q_1.AddTransition (1, 1, 0) q_1
    q_1.AddTransition (1, 1, 1) q_1
    carry.AddTransition (0, 0, 1) noCarry
    carry.AddTransition (0, 0, 0) q_1
    carry.AddTransition (0, 1, 1) q_1
    carry.AddTransition (1, 1, 0) q_1
    carry.AddTransition (1, 0, 1) q_1
    carry.AddTransition (0, 1, 0) carry
    carry.AddTransition (1, 0, 0) carry
    carry.AddTransition (1, 1, 1) carry
    DFA noCarry

let dfa_not_eq = DFA.complement dfa_eq

let dfa_less_eq = DFA.union dfa_eq dfa_less
