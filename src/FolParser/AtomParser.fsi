module FolParser.AtomParser

open FParsec
open Ast

val parseAtom: (CharStream<unit> -> Reply<Atom>)
