module Ast.Passes

open Ast.Ast

let assignId scope =
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
        | Const v -> Const <| Common.intToBits v
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

open Ident

let grab_names =
    let filter fst snd =
        List.filter (fun item -> not <| List.exists ((=) item) snd) fst
        |> List.append snd in

    let rec term (arg: Term<id, _>) =
        match arg with
        | BitwiseMinimum(left, right)
        | Plus(left, right)
        | Mult(left, right) -> filter (term left) (term right)
        | Const v -> []
        | Var name -> [ name ]

    let rec atom =
        function
        | (True | False) -> []
        | Equals(left, right)
        | Less(left, right)
        | Greater(left, right) -> filter (term left) (term right)

    let rec liter =
        function
        | BareAtom a -> atom a
        | Not l -> liter l
        | And(left, right)
        | Or(left, right)
        | Implies(left, right) -> filter (liter left) (liter right)
        | Exists(_, next)
        | Forall(_, next) -> liter next

    liter


let rec goToTerm (term: Term<id, _>) vars newVars atoms =
    let makeEq t x y z = Equals(t (x, y), z)

    match term with
    | Var a -> [ Var a ], [], []
    | Plus(term1, term2) ->
        match (term1, term2) with
        | Var a, Var b -> [ Var a; Var b ], [ Var(Id(0, "e0")) ], [ makeEq Plus (Var a) (Var b) (Var(Id(0, "e0"))) ]
        | Plus(termP1, Var b), Var c ->
            let vars, newVars, atoms = goToTerm (Plus(termP1, Var b)) vars newVars atoms
            let ve = newVars |> List.last
            let nve = Var(Id(newVars.Length, $"e{newVars.Length}"))
            vars @ [ Var c ], newVars @ [ nve ], atoms @ [ makeEq Plus ve (Var c) nve ]
        | _ -> vars, newVars, atoms
    | _ -> vars, newVars, atoms

let truncateTerms (literal: Literal<id, _>) =
    match literal with
    | BareAtom atom ->
        match atom with
        | Equals(term1, term2) ->
            match term2 with
            | Var b ->
                match term1 with
                | Var a -> literal
                | _ ->
                    let v, nv, atoms = (goToTerm term1 [] [] [])

                    if nv.Length = 1 then
                        literal
                    else
                        Exists(
                            (nv
                             |> List.map (fun x ->
                                 let (Var e) = x
                                 e)),
                            List.foldBack
                                (fun a acc -> And(BareAtom a, acc))
                                atoms
                                (BareAtom(Equals(List.last nv, Var b)))
                        )
            | _ -> literal
        | _ -> literal
    | _ -> literal
