module FolParser.TermParser

open FParsec
open Ast

val parseTerm: (CharStream<unit> -> Reply<Term>)
