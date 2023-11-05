module PeanutProver.CLI.Translate

open Ast

let assign scope =
    let rec term (scope: Ident.Scope) expr =
        let term = term scope in

        match expr with
        | BitwiseMinimum(left, right) -> BitwiseMinimum(term left, term right)
        | Plus(left, right) -> Plus(term left, term right)
        | Mult(left, right) -> Mult(term left, term right)
        | Const c -> Const c
        | Var name -> Var <| scope.Look(name)

    let rec atom scope expr =
        match expr with
        | True -> True
        | False -> False
        | Equals(left, right) -> Equals <| (term scope left, term scope right)
        | Less(left, right) -> Less(term scope left, term scope right)
        | Greater(left, right) -> Greater(term scope left, term scope right)

    let rec liter (scope: Ident.Scope) lit =
        let sameScope = liter scope in

        match lit with
        | BareAtom a -> BareAtom <| atom scope a
        | Not l -> Not <| sameScope l
        | And(left, right) -> (sameScope left, sameScope right) |> And
        | Or(left, right) -> (sameScope left, sameScope right) |> Or
        | Implies(left, right) -> (sameScope left, sameScope right) |> Implies
        | Exists(names, next) ->
            let newScope = scope.Enter(names) in
            let names = List.map newScope.Look names in
            (names, liter newScope next) |> Exists
        | Forall(names, next) ->
            // copypast see above
            let newScope = scope.Enter(names) in
            let names = List.map newScope.Look names in
            (names, liter newScope next) |> Forall in

    liter scope

let substituteConstant globals values =
    let map =
        List.fold (fun acc (name, value) -> Map.add name value acc) Map.empty
        <| List.zip globals values in

    let getValue key =
        Map.tryFind key map
        |> Option.defaultWith (fun () -> failwith "Undefined global variable") in

    let isGlobal name = List.exists ((=) name) globals in

    let rec term =
        function
        | BitwiseMinimum(left, right) -> BitwiseMinimum(term left, term right)
        | Plus(left, right) -> Plus(term left, term right)
        | Mult(left, right) -> Mult(term left, term right)
        | Const _ -> failwith "not supported"
        | Var name -> if isGlobal name then Const <| getValue name else Var name

    let rec atom =
        function
        | True -> True
        | False -> False
        | Equals(left, right) -> Equals <| (term left, term right)
        | Less(left, right) -> Less(term left, term right)
        | Greater(left, right) -> Greater(term left, term right)

    let rec liter =
        function
        | BareAtom a -> BareAtom <| atom a
        | Not l -> Not <| liter l
        | And(left, right) -> (liter left, liter right) |> And
        | Or(left, right) -> (liter left, liter right) |> Or
        | Implies(left, right) -> (liter left, liter right) |> Implies
        | Exists(names, next) -> (names, liter next) |> Exists
        | Forall(names, next) -> (names, liter next) |> Forall in

    liter
