module FolParser.LiteralParser

open FParsec
open Fol

val parseLiteral: (CharStream<unit> -> Reply<Literal>)
