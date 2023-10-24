module FolParser.CommonParsers

open FParsec

let ws = spaces
let strWs s = pstring s >>. ws
