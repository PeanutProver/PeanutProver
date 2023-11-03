namespace Ast

open System.Collections

module Ident =
    type id = Id of int * string
    
    let private stack = Stack()
    
    type Scope(init, map) =
        let map = map 
        let mutable counter = init
        member _.Enter(names: string list) =
            List.fold (fun map name ->
                counter <- counter + 1
                Map.add name counter map) map names
            |> fun newMap ->
                Scope(counter, newMap)
            
        member _.Look(name) =
            Map.tryFind name map
            |> Option.defaultWith (fun () ->
                failwith $"Undefined name %s{name}")
            |> fun value -> Id (value, name)
            
    let start = Scope(0, Map.empty)
        

type ('a, 'b) Term =
    | BitwiseMinimum of  Term<'a, 'b> * Term<'a, 'b>
    | Plus of Term<'a, 'b> * Term<'a, 'b>
    | Mult of Term<'a, 'b> * Term<'a, 'b> // Left argument can only Const, otherwise we must fail
    | Const of 'b
    | Var of 'a 

type  Atom<'a, 'b> =
    | True
    | False
    | Equals of Term<'a, 'b> * Term<'a, 'b>
    | Less of Term<'a, 'b> * Term<'a, 'b>
    | Greater of Term<'a, 'b> *  Term<'a, 'b>

type Literal<'a, 'b> =
    | BareAtom of Atom<'a, 'b>
    | Not of Literal<'a, 'b>
    | And of Literal<'a, 'b> * Literal<'a, 'b>
    | Or of Literal<'a, 'b> * Literal<'a, 'b>
    | Implies of Literal<'a, 'b> * Literal<'a, 'b>
    | Exists of 'a list * Literal<'a, 'b>
    | Forall of 'a list * Literal<'a, 'b>
