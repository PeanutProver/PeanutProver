module FolParser.CommonParsers

open FParsec

let ws = spaces
let strWs s = pstring s >>. ws

let identifier: Parser<string, unit> =
    let isIdentifierFirstChar c = isLetter c
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'

    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> ws // skips trailing whitespace

