module Ast.Ast

type ('a, 'b) Term =
    | BitwiseMinimum of Term<'a, 'b> * Term<'a, 'b>
    | Plus of Term<'a, 'b> * Term<'a, 'b>
    | Mult of Term<'a, 'b> * Term<'a, 'b> // Left argument can only Const, otherwise we must fail
    | Const of 'b
    | Var of 'a

type Atom<'a, 'b> =
    | True
    | False
    | Equals of Term<'a, 'b> * Term<'a, 'b>
    | Less of Term<'a, 'b> * Term<'a, 'b>
    | Greater of Term<'a, 'b> * Term<'a, 'b>

type Literal<'a, 'b> =
    | BareAtom of Atom<'a, 'b>
    | Not of Literal<'a, 'b>
    | And of Literal<'a, 'b> * Literal<'a, 'b>
    | Or of Literal<'a, 'b> * Literal<'a, 'b>
    | Implies of Literal<'a, 'b> * Literal<'a, 'b>
    | Exists of 'a list * Literal<'a, 'b>
    | Forall of 'a list * Literal<'a, 'b>
