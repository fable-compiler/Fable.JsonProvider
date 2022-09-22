module rec ProviderDsl

open System.Reflection
open FSharp.Quotations
open ProviderImplementation.ProvidedTypes

type Member =
    | ChildType of System.Type
    | Property of name: string * typ: ErasedType * isStatic: bool * body: (Expr list -> Expr)
    | Method of name: string * args: (string * ErasedType) list * typ: ErasedType * isStatic: bool * body: (Expr list -> Expr)
    | Constructor of args: (string * ErasedType) list * body: (Expr list -> Expr)

type ErasedType =
    | Any
    | Bool
    | Int
    | Float
    | String
    | Array of ErasedType
    | Option of ErasedType
    | Custom of System.Type
    | Tuple of ErasedType list

let addMembers (t: ProvidedTypeDefinition) members =
    for memb in members do
        let memb: MemberInfo =
            match memb with
            | ChildType t ->
                upcast t
            | Property(name, typ, isStatic, body) ->
                upcast ProvidedProperty(name, makeType typ, isStatic = isStatic, getterCode = body)
            | Method(name, args, typ, isStatic, body) ->
                let args = args |> List.map (fun (name, t) -> ProvidedParameter(name, makeType t))
                upcast ProvidedMethod(name, args, makeType typ, isStatic = isStatic, invokeCode = body)
            | Constructor(args, body) ->
                let args = args |> List.map (fun (name, t) -> ProvidedParameter(name, makeType t))
                upcast ProvidedConstructor(args, invokeCode = body)
        t.AddMember(memb)

let makeType = function
    | Any -> typeof<obj>
    | Bool -> typeof<bool>
    | Int -> typeof<int>
    | Float -> typeof<float>
    | String -> typeof<string>
    | Array t -> (makeType t).MakeArrayType()
    | Option t -> typedefof<Option<obj>>.MakeGenericType(makeType t)
    | Custom t -> t
    | Tuple ts ->
        match ts with
        | [] | [_] -> failwith "Tuple with only one or none items"
        | [t1; t2] -> typedefof<obj * obj>.MakeGenericType(makeType t1, makeType t2)
        | [t1; t2; t3] -> typedefof<obj * obj * obj>.MakeGenericType(makeType t1, makeType t2, makeType t3)
        | [t1; t2; t3; t4] -> typedefof<obj * obj * obj * obj>.MakeGenericType(makeType t1, makeType t2, makeType t3, makeType t4)
        | _ -> failwith "TODO: Tuples of more than 4 items"

let makeCustomType(name: string, members: Member seq): System.Type =
    let t = ProvidedTypeDefinition(name, baseType = Some typeof<obj>, hideObjectMethods = true, isErased = true)
    addMembers t members
    upcast t

let makeRootType(assembly: Assembly, nameSpace: string, typeName: string, members: Member seq) =
    let root = ProvidedTypeDefinition(assembly, nameSpace, typeName, baseType = Some typeof<obj>, hideObjectMethods = true, isErased = true)
    addMembers root members
    root
