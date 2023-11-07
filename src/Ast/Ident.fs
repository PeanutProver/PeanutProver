module Ast.Ident

open System.Collections
type id = Id of int * string

let private stack = Stack()

type Scope(init, map) =
    let map = map
    let mutable counter = init

    member _.Enter(names: string list) =
        List.fold
            (fun map name ->
                counter <- counter + 1
                Map.add name counter map)
            map
            names
        |> fun newMap -> Scope(counter, newMap)

    member _.Look(name) =
        Map.tryFind name map
        |> Option.defaultWith (fun () -> failwith $"Undefined name %s{name}")
        |> fun value -> Id(value, name)

let start = Scope(0, Map.empty)
