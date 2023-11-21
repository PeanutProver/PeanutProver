module Ast.Common

open System

type bit =
    | Zero
    | One


let intToBits (x: int) =
    x
    |> (fun x -> Convert.ToString(x, 2))
    |> Seq.map (function
        | '1' -> One
        | '0' -> Zero
        | _ -> failwith "Unexpected bit value")

let strToBits (x: string) = x |> Convert.ToInt32 |> intToBits
