module FolParser.LiteralParser

open FParsec
open Ast

val parseLiteral: Parser<Literal<_, _>, unit>
