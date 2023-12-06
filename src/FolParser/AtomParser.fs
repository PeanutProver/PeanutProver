module FolParser.AtomParser

open FParsec
open CommonParsers
open Ast.Ast
open TermParser

let pTrue = choice [ stringCIReturn "true" True; stringReturn "⊤" True ] .>> ws

let pFalse =
    choice [ stringCIReturn "false" False; stringCIReturn "⊥" False ] .>> ws

let pAutomaton =
    tuple2 (strWs "$" >>. identifier) (between (strWs "(") (strWs ")") (many parseTerm))
    |>> Automaton

let parseAtom =
    choice
        [ pTrue
          pFalse
          pAutomaton
          pipe2 (parseTerm .>>? strWs "=") parseTerm (fun lhs rhs -> Equals(lhs, rhs))
          pipe2 (parseTerm .>>? strWs "<") parseTerm (fun lhs rhs -> Less(lhs, rhs))
          pipe2 (parseTerm .>>? strWs ">") parseTerm (fun lhs rhs -> Greater(lhs, rhs)) ]
