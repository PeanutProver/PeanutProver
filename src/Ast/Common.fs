module Ast.Common

open System

type bit =
    | Z
    | O


let intToBits (x: int) =
    x
    |> (fun x -> Convert.ToString(x, 2))
    |> Seq.map (function
        | '1' -> O
        | '0' -> Z
        | _ -> failwith "Unexpected bit value")

let strToBits (x: string) = x |> Convert.ToInt32 |> intToBits
