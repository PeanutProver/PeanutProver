module Tests

open Xunit
open FParsec
open Ast
open FolParser.LiteralParser
open FsUnit.Xunit

let parse = run (parseLiteral .>> eof)

// Plain text tests

[<Fact>]
let ``\exists x (3*y+1 < x /\ 2*x > y+4 /\ 3*x < 1)`` () =
    match (parse """\exists x (3*y+1 < x /\ 2*x > y+4 /\ 3*x < 1)""") with
    | Success(literal, _, _) ->
        literal
        |> should
            equal
            (Exists(
                [ "x" ],
                And(
                    And(
                        BareAtom(Less(Plus(Mult(Const 3, Var "y"), Const 1), Var "x")),
                        BareAtom(Greater(Mult(Const 2, Var "x"), Plus(Var "y", Const 4)))
                    ),
                    BareAtom(Less(Mult(Const 3, Var "x"), Const 1))
                )
            ))
    | Failure(errorMessage, _, _) -> errorMessage |> should be EmptyString

[<Fact>]
let ``\exists x 3*y+1 < x /\ 2*x > y+4 /\ 3*x < 1`` () =
    match (parse """\exists x 3*y+1 < x /\ 2*x > y+4 /\ 3*x < 1""") with
    | Success(literal, _, _) ->
        literal
        |> should
            equal
            (And(
                And(
                    Exists([ "x" ], BareAtom(Less(Plus(Mult(Const 3, Var "y"), Const 1), Var "x"))),
                    BareAtom(Greater(Mult(Const 2, Var "x"), Plus(Var "y", Const 4)))
                ),
                BareAtom(Less(Mult(Const 3, Var "x"), Const 1))
            ))
    | Failure(errorMessage, _, _) -> errorMessage |> should be EmptyString

[<Fact>]
let ``x = z & y \/ \forall t (2*t = x)`` () =
    match (parse """x = z & y \/ \forall t (2*t = x)""") with
    | Success(literal, _, _) ->
        literal
        |> should
            equal
            (Or(
                BareAtom(Equals(Var "x", BitwiseMinimum(Var "z", Var "y"))),
                Forall([ "t" ], BareAtom(Equals(Mult(Const 2, Var "t"), Var "x")))
            ))
    | Failure(errorMessage, _, _) -> errorMessage |> should be EmptyString

[<Fact>]
let ``false -> true /\ ~\forall z (~z = 1 /\ ~~abc > 1 -> z < 157)`` () =
    match (parse """false -> true /\ ~\forall z (~z = 1 /\ ~~abc > 1 -> z < 157)""") with
    | Success(literal, _, _) ->
        literal
        |> should
            equal
            (Implies(
                BareAtom Atom.False,
                And(
                    BareAtom Atom.True,
                    Not(
                        Forall(
                            [ "z" ],
                            Implies(
                                And(
                                    Not(BareAtom(Equals(Var "z", Const 1))),
                                    Not(Not(BareAtom(Greater(Var "abc", Const 1))))
                                ),
                                BareAtom(Less(Var "z", Const 157))
                            )
                        )
                    )
                )
            ))
    | Failure(errorMessage, _, _) -> errorMessage |> should be EmptyString


[<Fact>]
let ``(false -> true) /\ ~\forall z (~z = 1 /\ ~~abc > 1 -> z < 157)`` () =
    match (parse """(false -> true) /\ ~\forall z (~z = 1 /\ ~~abc > 1 -> z < 157)""") with
    | Success(literal, _, _) ->
        literal
        |> should
            equal
            (And(
                Implies(BareAtom Atom.False, BareAtom Atom.True),
                Not(
                    Forall(
                        [ "z" ],
                        Implies(
                            And(
                                Not(BareAtom(Equals(Var "z", Const 1))),
                                Not(Not(BareAtom(Greater(Var "abc", Const 1))))
                            ),
                            BareAtom(Less(Var "z", Const 157))
                        )
                    )
                )
            ))
    | Failure(errorMessage, _, _) -> errorMessage |> should be EmptyString

// Unicode parse tests

[<Fact>]
let ``∃ x (3*y+1 < x ∧ 2*x > y+4 ∧ 3*x < 1)`` () =
    match (parse """∃ x (3*y+1 < x ∧ 2*x > y+4 ∧ 3*x < 1)""") with
    | Success(literal, _, _) ->
        literal
        |> should
            equal
            (Exists(
                [ "x" ],
                And(
                    And(
                        BareAtom(Less(Plus(Mult(Const 3, Var "y"), Const 1), Var "x")),
                        BareAtom(Greater(Mult(Const 2, Var "x"), Plus(Var "y", Const 4)))
                    ),
                    BareAtom(Less(Mult(Const 3, Var "x"), Const 1))
                )
            ))
    | Failure(errorMessage, _, _) -> errorMessage |> should be EmptyString

[<Fact>]
let ``∃ x 3*y+1 < x ∧ 2*x > y+4 ∧ 3*x < 1`` () =
    match (parse """∃ x 3*y+1 < x ∧ 2*x > y+4 ∧ 3*x < 1""") with
    | Success(literal, _, _) ->
        literal
        |> should
            equal
            (And(
                And(
                    Exists([ "x" ], BareAtom(Less(Plus(Mult(Const 3, Var "y"), Const 1), Var "x"))),
                    BareAtom(Greater(Mult(Const 2, Var "x"), Plus(Var "y", Const 4)))
                ),
                BareAtom(Less(Mult(Const 3, Var "x"), Const 1))
            ))
    | Failure(errorMessage, _, _) -> errorMessage |> should be EmptyString

[<Fact>]
let ``x = z & y ∨ ∀ t (2*t = x)`` () =
    match (parse """x = z & y ∨ ∀ t (2*t = x)""") with
    | Success(literal, _, _) ->
        literal
        |> should
            equal
            (Or(
                BareAtom(Equals(Var "x", BitwiseMinimum(Var "z", Var "y"))),
                Forall([ "t" ], BareAtom(Equals(Mult(Const 2, Var "t"), Var "x")))
            ))
    | Failure(errorMessage, _, _) -> errorMessage |> should be EmptyString

[<Fact>]
let ``⊥ → ⊤ ∧ ¬∀ z (¬z = 1 ∧ ¬¬abc > 1 → z < 157)`` () =
    match (parse """⊥ → ⊤ ∧ ¬∀ z (¬z = 1 ∧ ¬¬abc > 1 → z < 157)""") with
    | Success(literal, _, _) ->
        literal
        |> should
            equal
            (Implies(
                BareAtom Atom.False,
                And(
                    BareAtom Atom.True,
                    Not(
                        Forall(
                            [ "z" ],
                            Implies(
                                And(
                                    Not(BareAtom(Equals(Var "z", Const 1))),
                                    Not(Not(BareAtom(Greater(Var "abc", Const 1))))
                                ),
                                BareAtom(Less(Var "z", Const 157))
                            )
                        )
                    )
                )
            ))
    | Failure(errorMessage, _, _) -> errorMessage |> should be EmptyString


[<Fact>]
let ``(⊥ → ⊤) ∧ ¬∀ z (¬z = 1 ∧ ¬¬abc > 1 → z < 157)`` () =
    match (parse """(⊥ → ⊤) ∧ ¬∀ z (¬z = 1 ∧ ¬¬abc > 1 → z < 157)""") with
    | Success(literal, _, _) ->
        literal
        |> should
            equal
            (And(
                Implies(BareAtom Atom.False, BareAtom Atom.True),
                Not(
                    Forall(
                        [ "z" ],
                        Implies(
                            And(
                                Not(BareAtom(Equals(Var "z", Const 1))),
                                Not(Not(BareAtom(Greater(Var "abc", Const 1))))
                            ),
                            BareAtom(Less(Var "z", Const 157))
                        )
                    )
                )
            ))
    | Failure(errorMessage, _, _) -> errorMessage |> should be EmptyString
