module FolParser.AtomParser

open FParsec
open Fol

val parseAtom: (CharStream<unit> -> Reply<Atom>)
