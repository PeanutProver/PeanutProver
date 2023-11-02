module FolParserTests.DFATests

open PeanutProver.Automata
open PeanutProver.DFA
open Xunit

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
            dfa.Recognize[[ 1; 0 ]
                          [ 0; 0 ]
                          [ 1; 1 ]]
        )
    )

[<Fact>]
let ``DFA Union 1`` () =
    let rnd = System.Random()
    let dfa1 = PredefinedAutomata.dfa_eq
    let dfa2 = PredefinedAutomata.dfa_less
    let dfa3 = DFA.union dfa1 dfa2

    for i in 1..10 do
        let input = List.init i (fun _ -> [ rnd.Next(0, 1); rnd.Next(0, 1) ])

        Assert.Equal(
            DfaResultToBool(dfa3.Recognize input),
            (DfaResultToBool(dfa1.Recognize input) || DfaResultToBool(dfa2.Recognize input))
        )

        Assert.Equal(
            DfaResultToBool(dfa3.Recognize [ [ 0; 0 ]; [ 0; 0 ]; [ 1; 1 ] ]),
            (DfaResultToBool(dfa1.Recognize input) || DfaResultToBool(dfa2.Recognize input))
        )
