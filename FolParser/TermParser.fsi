module FolParser.TermParser

open FParsec
open Fol

val parseTerm: (CharStream<unit> -> Reply<Term>)
