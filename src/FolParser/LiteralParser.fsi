module FolParser.LiteralParser

open FParsec
open Ast

val parseLiteral: (CharStream<unit> -> Reply<Literal>)
