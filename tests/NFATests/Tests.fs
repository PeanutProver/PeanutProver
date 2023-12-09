module FolParserTests.NFATests

open PeanutProver.Automata
open PeanutProver.NFA
open Xunit
open System
open Ast.Common

let NfaResultToBool result =
    match result with
    | Accept -> true
    | Fail _ -> false

[<Fact>]
let ``NFA Not Equal`` () =
    let nfa = PredefinedAutomata.nfa_not_eq

    Assert.True(
        NfaResultToBool(
            nfa.Recognize[[ One; Zero ]
                          [ Zero; Zero ]
                          [ One; One ]]
        )
    )

[<Fact>]
let ``NFA Union 1`` () =
    let nfa1 = PredefinedAutomata.nfa_eq
    let nfa2 = PredefinedAutomata.nfa_less
    let nfa3 = NFA.union nfa1 nfa2

    let genBinaryChar () =
        (Random().Next(0, 1))
        |> function
            | 0 -> Zero
            | 1 -> One
            | _ -> failwith "Int not always bit"

    for i in 1..10 do
        let input = List.init i (fun _ -> [ genBinaryChar (); genBinaryChar () ])

        Assert.Equal(
            NfaResultToBool(nfa3.Recognize input),
            (NfaResultToBool(nfa1.Recognize input) || NfaResultToBool(nfa2.Recognize input))
        )

        Assert.Equal(
            NfaResultToBool(nfa3.Recognize [ [ Zero; Zero ]; [ Zero; Zero ]; [ One; One ] ]),
            (NfaResultToBool(nfa1.Recognize input) || NfaResultToBool(nfa2.Recognize input))
        )
