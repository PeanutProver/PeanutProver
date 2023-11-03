module PeanutProver.DFA.PredefinedAutomata

open PeanutProver.Automata

let dfa_eq =
    let startState = State<_>("eq:Start", true, true)
    let q_1 = State<_>("eq:q_1", false, false)
    startState.AddTransition [ Zero; Zero ] startState
    startState.AddTransition [ One; One ] startState
    startState.AddTransition [ One; Zero ] q_1
    startState.AddTransition [ Zero; One ] q_1
    q_1.AddTransition [ Zero; Zero ] q_1
    q_1.AddTransition [ One; Zero ] q_1
    q_1.AddTransition [ Zero; One ] q_1
    q_1.AddTransition [ One; One ] q_1
    DFA startState

let dfa_less =
    let startState = State< _>("less:Start", true, false)
    let q_1 = State< _>("less:q_1", false, true)
    startState.AddTransition [ Zero; One ] q_1
    startState.AddTransition [ One; One ] startState
    startState.AddTransition [ One; Zero ] startState
    startState.AddTransition [ Zero; Zero ] startState
    q_1.AddTransition [ One; Zero ] startState
    q_1.AddTransition [ Zero; One ] q_1
    q_1.AddTransition [ One; One ] q_1
    q_1.AddTransition [ Zero; Zero ] q_1
    DFA startState

let dfa_sum =
    let noCarry = State< _>("sum:noCarry", true, true)
    let carry = State< _>("sum:carry", false, false)
    let q_1 = State< _>("sum:q_1", false, false)
    noCarry.AddTransition [ Zero; Zero; Zero ] noCarry
    noCarry.AddTransition [ Zero; One; One ] noCarry
    noCarry.AddTransition [ One; Zero; One ] noCarry
    noCarry.AddTransition [ One; One; Zero ] carry
    noCarry.AddTransition [ Zero; One; Zero ] q_1
    noCarry.AddTransition [ One; Zero; Zero ] q_1
    noCarry.AddTransition [ Zero; Zero; One ] q_1
    noCarry.AddTransition [ One; One; One ] q_1
    q_1.AddTransition [ Zero; Zero; Zero ] q_1
    q_1.AddTransition [ Zero; Zero; One ] q_1
    q_1.AddTransition [ Zero; One; Zero ] q_1
    q_1.AddTransition [ Zero; One; One ] q_1
    q_1.AddTransition [ One; Zero; Zero ] q_1
    q_1.AddTransition [ One; Zero; One ] q_1
    q_1.AddTransition [ One; One; Zero ] q_1
    q_1.AddTransition [ One; One; One ] q_1
    carry.AddTransition [ Zero; Zero; One ] noCarry
    carry.AddTransition [ Zero; Zero; Zero ] q_1
    carry.AddTransition [ Zero; One;One ] q_1
    carry.AddTransition [ One; One; Zero ] q_1
    carry.AddTransition [ One; Zero; One ] q_1
    carry.AddTransition [ Zero; One; Zero ] carry
    carry.AddTransition [ One; Zero; Zero ] carry
    carry.AddTransition [ One; One; One ] carry
    DFA noCarry

let dfa_not_eq = DFA.complement dfa_eq

let dfa_less_eq = DFA.union dfa_eq dfa_less

let dfa_bitwise_minimum =
    let start = State<_>("bm:Start", true, true)
    let q_1 = State<_>("bm:q_1", false, false)
    start.AddTransition [ Zero; Zero; Zero ] start
    start.AddTransition [ Zero; One; One ] q_1
    start.AddTransition [ One; Zero; One ] q_1
    start.AddTransition [ One; One; Zero ] q_1
    start.AddTransition [ Zero; One; Zero ] start
    start.AddTransition [ One; Zero; Zero ] start
    start.AddTransition [ Zero; Zero; One ] q_1
    start.AddTransition [ One; One; One ] start
    q_1.AddTransition [ Zero; Zero; Zero ] q_1
    q_1.AddTransition [ Zero; Zero; One ] q_1
    q_1.AddTransition [ Zero; One; Zero ] q_1
    q_1.AddTransition [ Zero; One; One ] q_1
    q_1.AddTransition [ One; Zero; Zero ] q_1
    q_1.AddTransition [ One; Zero; One ] q_1
    q_1.AddTransition [ One; One; Zero ] q_1
    q_1.AddTransition [ One; One; One ] q_1
    DFA start
