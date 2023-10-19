module FolParser.AtomParser

open FParsec
open CommonParsers
open Fol

let pTrue = choice [ stringCIReturn "true" True; stringReturn "⊤" True ]
let pFalse = choice [ stringCIReturn "false" False; stringCIReturn "⊥" False ]

let parseAtom =
    choice
        [ pTrue
          pFalse
          pipe2 (TermParser.parseTerm .>>? pstring "=") TermParser.parseTerm (fun lhs rhs -> Equals(lhs, rhs))
          pipe2 (TermParser.parseTerm .>>? strWs "<") TermParser.parseTerm (fun lhs rhs -> Less(lhs, rhs))
          pipe2 (TermParser.parseTerm .>>? strWs ">") TermParser.parseTerm (fun lhs rhs -> Less(rhs, lhs)) ]
