namespace Fol

type Atom =
    | True
    | False
    | Equals of Term * Term
    | Less of Term * Term
