module FolParserTests.DFATests

open PeanutProver.Automata
open PeanutProver.DFA
open Xunit
open System
open Ast.Common

let DfaResultToBool result =
    match result with
    | Accept _ -> true
    | Fail _ -> false
    | Partial _ -> false

[<Fact>]
let ``DFA Not Equal`` () =
    let dfa = PredefinedAutomata.dfa_not_eq

    Assert.True(
        DfaResultToBool(
            dfa.Recognize[[ One; Zero ]
                          [ Zero; Zero ]
                          [ One; One ]]
        )
    )

[<Fact>]
let ``DFA Union 1`` () =
    let dfa1 = PredefinedAutomata.dfa_eq
    let dfa2 = PredefinedAutomata.dfa_less
    let dfa3 = DFA.union dfa1 dfa2

    let genBinaryChar () =
        (Random().Next(0, 1))
        |> function
            | 0 -> Zero
            | 1 -> One
            | _ -> failwith "Int not always bit"

    for i in 1..10 do
        let input = List.init i (fun _ -> [ genBinaryChar (); genBinaryChar () ])

        Assert.Equal(
            DfaResultToBool(dfa3.Recognize input),
            (DfaResultToBool(dfa1.Recognize input) || DfaResultToBool(dfa2.Recognize input))
        )

        Assert.Equal(
            DfaResultToBool(dfa3.Recognize [ [ Zero; Zero ]; [ Zero; Zero ]; [ One; One ] ]),
            (DfaResultToBool(dfa1.Recognize input) || DfaResultToBool(dfa2.Recognize input))
        )
