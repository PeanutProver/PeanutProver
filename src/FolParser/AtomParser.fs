module FolParser.AtomParser

open FParsec
open CommonParsers
open Ast

let pTrue = choice [ stringCIReturn "true" True; stringReturn "⊤" True ]
let pFalse = choice [ stringCIReturn "false" False; stringCIReturn "⊥" False ]

let parseAtom =
    choice
        [ pTrue .>> ws
          pFalse .>> ws
          pipe2 (TermParser.parseTerm .>>? strWs "=") TermParser.parseTerm (fun lhs rhs -> Equals(lhs, rhs))
          pipe2 (TermParser.parseTerm .>>? strWs "<") TermParser.parseTerm (fun lhs rhs -> Less(lhs, rhs))
          pipe2 (TermParser.parseTerm .>>? strWs ">") TermParser.parseTerm (fun lhs rhs -> Greater(lhs, rhs)) ]
