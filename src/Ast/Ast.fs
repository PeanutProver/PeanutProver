module Ast.Ast

open System.Text

type Term<'a, 'b> =
    | BitwiseMinimum of Term<'a, 'b> * Term<'a, 'b>
    | Plus of Term<'a, 'b> * Term<'a, 'b>
    | Mult of Term<'a, 'b> * Term<'a, 'b> // Left argument can only Const, otherwise we must fail
    | Const of 'b
    | Var of 'a

    override this.ToString() =
        match this with
        | Var name -> name.ToString()
        | Const value -> value.ToString()
        | Plus(lhs, rhs) -> $"({lhs} + {rhs})"
        | Mult(lhs, rhs) -> $"({lhs} * {rhs})"
        | BitwiseMinimum(lhs, rhs) -> $"({lhs} & {rhs})"

type Atom<'a, 'b> =
    | True
    | False
    | Equals of Term<'a, 'b> * Term<'a, 'b>
    | Less of Term<'a, 'b> * Term<'a, 'b>
    | Greater of Term<'a, 'b> * Term<'a, 'b>
    | Automaton of string * Term<'a, 'b> list

    override this.ToString() =
        match this with
        | True -> "⊤"
        | False -> "⊥"
        | Equals(lhs, rhs) -> $"{lhs} = {rhs}"
        | Less(lhs, rhs) -> $"{lhs} < {rhs}"
        | Greater(lhs, rhs) -> $"{lhs} > {rhs}"
        | Automaton(name, terms) ->
            let termsConcat = StringBuilder().AppendJoin(", ", terms).ToString()
            $"${name}({termsConcat})"

type Literal<'a, 'b> =
    | BareAtom of Atom<'a, 'b>
    | Not of Literal<'a, 'b>
    | And of Literal<'a, 'b> * Literal<'a, 'b>
    | Or of Literal<'a, 'b> * Literal<'a, 'b>
    | Implies of Literal<'a, 'b> * Literal<'a, 'b>
    | Exists of 'a list * Literal<'a, 'b>
    | Forall of 'a list * Literal<'a, 'b>

    override this.ToString() =
        match this with
        | BareAtom atom -> atom.ToString()
        | Not literal -> $"¬({literal})"
        | And(lhs, rhs) -> $"({lhs} ∧ {rhs})"
        | Or(lhs, rhs) -> $"({lhs} ∨ {rhs})"
        | Implies(lhs, rhs) -> $"({lhs} → {rhs})"
        | Exists(vars, literal) ->
            let varsConcat = StringBuilder().AppendJoin(", ", vars).ToString()
            $"∃ {varsConcat} ({literal})"
        | Forall(vars, literal) ->
            let varsConcat = StringBuilder().AppendJoin(", ", vars).ToString()
            $"∀ {varsConcat} ({literal})"
