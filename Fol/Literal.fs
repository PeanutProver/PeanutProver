namespace Fol

type Literal =
    | BareAtom of Atom
    | Not of Literal
    | And of Literal * Literal
    | Or of Literal * Literal
    | Implies of Literal * Literal
    | Exists of string list * Literal
    | Forall of string list * Literal
