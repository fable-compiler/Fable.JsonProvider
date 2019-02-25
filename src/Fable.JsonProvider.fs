namespace Fable.Core

type EmitAttribute(macro: string) =
    inherit System.Attribute()

namespace Fable

module JsonProvider =

    open FSharp.Quotations
    open FSharp.Core.CompilerServices
    open ProviderImplementation.ProvidedTypes

    open ProviderDsl
    open Fable.Core

    [<Emit("JSON.parse($0)")>]
    let jsonParse (json: string) = obj()

    [<Emit("$0[$1]")>]
    let getProp (o: obj) (k: string) = obj()

    let firstToUpper (s: string) =
        s.[0..0].ToUpper() + s.[1..]

    let getterCode name =
        fun (args: Expr list) -> <@@ getProp %%args.Head name @@>

    let rec makeType typeName json =
        match json with
        | JsonParser.Null -> Any
        | JsonParser.Bool _ -> Bool
        | JsonParser.Number _ -> Float
        | JsonParser.String _ -> String
        | JsonParser.Array items ->
            match items with
            | [] -> Array Any
            // TODO: Check if all items have same type
            | item::_ -> makeType typeName item |> Array
        | JsonParser.Object members ->
            let members = members |> List.collect (makeMember typeName)
            makeCustomType(typeName, members) |> Custom

    and makeMember ns (name, json) =
        let t = makeType (firstToUpper name) json
        let m = Property(name, t, false, getterCode name)
        match t with
        | Custom t -> [ChildType t; m]
        | _ -> [m]

    [<TypeProvider>]
    type public JsonProvider (config : TypeProviderConfig) as this =
        inherit TypeProviderForNamespaces (config)
        let asm = System.Reflection.Assembly.GetExecutingAssembly()
        let ns = "Fable.JsonProvider"

        let staticParams = [ProvidedStaticParameter("sample",typeof<string>)]
        let generator = ProvidedTypeDefinition(asm, ns, "Generator", Some typeof<obj>, isErased = true)

        do generator.DefineStaticParameters(
            parameters = staticParams,
            instantiationFunction =  (fun typeName pVals ->
                    match pVals with 
                    | [| :? string as sample|] ->
                        match JsonParser.parse sample with
                        | Some(JsonParser.Object members) ->
                            makeRootType(asm, ns, typeName, [
                                yield! members |> List.collect (makeMember "")
                                yield Constructor(["json", String], fun args -> <@@ jsonParse %%args.Head @@>)
                            ])
                        | _ -> failwith "Sample is not a valid JSON object"
                    | _ -> failwith "unexpected parameter values"                
                )
            )

        do this.AddNamespace(ns, [generator])

    [<assembly:TypeProviderAssembly>]
    do ()