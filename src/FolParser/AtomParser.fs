module FolParser.AtomParser

open FParsec
open CommonParsers
open Ast

let pTrue = choice [ stringCIReturn "true" True; stringReturn "⊤" True ] .>> ws

let pFalse =
    choice [ stringCIReturn "false" False; stringCIReturn "⊥" False ] .>> ws

let parseAtom =
    choice
        [ pTrue
          pFalse
          pipe2 (TermParser.parseTerm .>>? strWs "=") TermParser.parseTerm (fun lhs rhs -> Equals(lhs, rhs))
          pipe2 (TermParser.parseTerm .>>? strWs "<") TermParser.parseTerm (fun lhs rhs -> Less(lhs, rhs))
          pipe2 (TermParser.parseTerm .>>? strWs ">") TermParser.parseTerm (fun lhs rhs -> Greater(lhs, rhs)) ]
