module FolParser.LiteralParser

open FParsec
open CommonParsers
open AtomParser
open Ast

let opp = OperatorPrecedenceParser<Literal, string list, unit>()
let expr = opp.ExpressionParser

let pVarList x =
    (ws >>. (many1 (many1Satisfy isAsciiLetter .>> ws .>> optional (strWs ",")))) x

let pEmpty = preturn [] .>> ws
let term = choice [ between (strWs "(") (strWs ")") expr; parseAtom |>> BareAtom ]

opp.TermParser <- term

opp.AddOperator(InfixOperator("->", pEmpty, 1, Associativity.Left, (fun lhs rhs -> Implies(lhs, rhs))))
opp.AddOperator(InfixOperator("→", pEmpty, 1, Associativity.Left, (fun lhs rhs -> Implies(lhs, rhs))))

opp.AddOperator(InfixOperator("\\/", pEmpty, 2, Associativity.Left, (fun lhs rhs -> Or(lhs, rhs))))
opp.AddOperator(InfixOperator("∨", pEmpty, 2, Associativity.Left, (fun lhs rhs -> Or(lhs, rhs))))

opp.AddOperator(InfixOperator("/\\", pEmpty, 2, Associativity.Left, (fun lhs rhs -> And(lhs, rhs))))
opp.AddOperator(InfixOperator("∧", pEmpty, 2, Associativity.Left, (fun lhs rhs -> And(lhs, rhs))))

opp.AddOperator(PrefixOperator("\\exists", pVarList, 3, false, (), (fun vars rhs -> Exists(vars, rhs))))
opp.AddOperator(PrefixOperator("∃", pVarList, 3, false, (), (fun vars rhs -> Exists(vars, rhs))))

opp.AddOperator(PrefixOperator("\\forall", pVarList, 3, false, (), (fun vars rhs -> Forall(vars, rhs))))
opp.AddOperator(PrefixOperator("∀", pVarList, 3, false, (), (fun vars rhs -> Forall(vars, rhs))))

opp.AddOperator(PrefixOperator("~", pEmpty, 10, true, Not))
opp.AddOperator(PrefixOperator("¬", pEmpty, 10, true, Not))
let parseLiteral = opp.ExpressionParser .>> ws
