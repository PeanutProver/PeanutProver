module FolParser.TermParser

open FParsec
open CommonParsers
open Ast

let opp = OperatorPrecedenceParser<Term<_, _>, unit, unit>()
let expr = opp.ExpressionParser

let term =
    choice
        [ between (strWs "(") (strWs ")") expr
          pint32 .>> ws |>> Const
          identifier |>> Var ]

opp.TermParser <- term

opp.AddOperator(InfixOperator("&", ws, 1, Associativity.Left, (fun lhs rhs -> BitwiseMinimum(lhs, rhs))))
opp.AddOperator(InfixOperator("+", ws, 2, Associativity.Left, (fun lhs rhs -> Plus(lhs, rhs))))
opp.AddOperator(InfixOperator("*", ws, 3, Associativity.Left, (fun lhs rhs -> Mult(lhs, rhs))))

let parseTerm = opp.ExpressionParser
