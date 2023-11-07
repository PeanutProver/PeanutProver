module PeanutProver.DFA.PredefinedAutomata

open PeanutProver.Automata
open Ast.Common 

let dfa_eq =
    let startState = State<_>("eq:Start", true, true)
    let q_1 = State<_>("eq:q_1", false, false)
    startState.AddTransition [ Z; Z ] startState
    startState.AddTransition [ O; O ] startState
    startState.AddTransition [ O; Z ] q_1
    startState.AddTransition [ Z; O ] q_1
    q_1.AddTransition [ Z; Z ] q_1
    q_1.AddTransition [ O; Z ] q_1
    q_1.AddTransition [ Z; O ] q_1
    q_1.AddTransition [ O; O ] q_1
    DFA startState

let dfa_less =
    let startState = State<_>("less:Start", true, false)
    let q_1 = State<_>("less:q_1", false, true)
    startState.AddTransition [ Z; O ] q_1
    startState.AddTransition [ O; O ] startState
    startState.AddTransition [ O; Z ] startState
    startState.AddTransition [ Z; Z ] startState
    q_1.AddTransition [ O; Z ] startState
    q_1.AddTransition [ Z; O ] q_1
    q_1.AddTransition [ O; O ] q_1
    q_1.AddTransition [ Z; Z ] q_1
    DFA startState

let dfa_sum =
    let noCarry = State<_>("sum:noCarry", true, true)
    let carry = State<_>("sum:carry", false, false)
    let q_1 = State<_>("sum:q_1", false, false)
    noCarry.AddTransition [ Z; Z; Z ] noCarry
    noCarry.AddTransition [ Z; O; O ] noCarry
    noCarry.AddTransition [ O; Z; O ] noCarry
    noCarry.AddTransition [ O; O; Z ] carry
    noCarry.AddTransition [ Z; O; Z ] q_1
    noCarry.AddTransition [ O; Z; Z ] q_1
    noCarry.AddTransition [ Z; Z; O ] q_1
    noCarry.AddTransition [ O; O; O ] q_1
    q_1.AddTransition [ Z; Z; Z ] q_1
    q_1.AddTransition [ Z; Z; O ] q_1
    q_1.AddTransition [ Z; O; Z ] q_1
    q_1.AddTransition [ Z; O; O ] q_1
    q_1.AddTransition [ O; Z; Z ] q_1
    q_1.AddTransition [ O; Z; O ] q_1
    q_1.AddTransition [ O; O; Z ] q_1
    q_1.AddTransition [ O; O; O ] q_1
    carry.AddTransition [ Z; Z; O ] noCarry
    carry.AddTransition [ Z; Z; Z ] q_1
    carry.AddTransition [ Z; O; O ] q_1
    carry.AddTransition [ O; O; Z ] q_1
    carry.AddTransition [ O; Z; O ] q_1
    carry.AddTransition [ Z; O; Z ] carry
    carry.AddTransition [ O; Z; Z ] carry
    carry.AddTransition [ O; O; O ] carry
    DFA noCarry

let dfa_not_eq = DFA.complement dfa_eq

let dfa_less_eq = DFA.union dfa_eq dfa_less

let dfa_bitwise_minimum =
    let start = State<_>("bm:Start", true, true)
    let q_1 = State<_>("bm:q_1", false, false)
    start.AddTransition [ Z; Z; Z ] start
    start.AddTransition [ Z; O; O ] q_1
    start.AddTransition [ O; Z; O ] q_1
    start.AddTransition [ O; O; Z ] q_1
    start.AddTransition [ Z; O; Z ] start
    start.AddTransition [ O; Z; Z ] start
    start.AddTransition [ Z; Z; O ] q_1
    start.AddTransition [ O; O; O ] start
    q_1.AddTransition [ Z; Z; Z ] q_1
    q_1.AddTransition [ Z; Z; O ] q_1
    q_1.AddTransition [ Z; O; Z ] q_1
    q_1.AddTransition [ Z; O; O ] q_1
    q_1.AddTransition [ O; Z; Z ] q_1
    q_1.AddTransition [ O; Z; O ] q_1
    q_1.AddTransition [ O; O; Z ] q_1
    q_1.AddTransition [ O; O; O ] q_1
    DFA start
