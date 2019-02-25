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

    let rec makeMember (name, json) =
        let getterCode (args: Expr list) =
            <@@ getProp %%args.Head name @@>
        match json with
        | JsonParser.Null -> None // Ignore
        | JsonParser.Bool _ -> Property(name, Bool, false, getterCode) |> Some
        | JsonParser.Number _ -> Property(name, Float, false, getterCode) |> Some
        | JsonParser.String _ -> Property(name, String, false, getterCode) |> Some
        // TODO: Check if all items have same type
        | JsonParser.Array _ -> Property(name, Array Any, false, getterCode) |> Some
        // TODO
        | JsonParser.Object _ -> None

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
                                yield! members |> List.choose makeMember
                                yield Constructor(["json", String], fun args -> <@@ jsonParse %%args.Head @@>)
                            ])
                        | _ -> failwith "Sample is not a valid JSON object"
                    | _ -> failwith "unexpected parameter values"                
                )
            )

        do this.AddNamespace(ns, [generator])

    [<assembly:TypeProviderAssembly>]
    do ()