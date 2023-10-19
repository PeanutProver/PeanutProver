namespace Fol

type Term =
    | BitwiseMinimum of Term * Term
    | Plus of Term * Term
    | Mult of Term * Term // Left argument can only Const, otherwise we must fail
    | Const of int
    | Var of string
