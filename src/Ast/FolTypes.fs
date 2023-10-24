namespace Ast

type Term =
    | BitwiseMinimum of Term * Term
    | Plus of Term * Term
    | Mult of Term * Term // Left argument can only Const, otherwise we must fail
    | Const of int
    | Var of string

type Atom =
    | True
    | False
    | Equals of Term * Term
    | Less of Term * Term
    | Greater of Term * Term

type Literal =
    | BareAtom of Atom
    | Not of Literal
    | And of Literal * Literal
    | Or of Literal * Literal
    | Implies of Literal * Literal
    | Exists of string list * Literal
    | Forall of string list * Literal
