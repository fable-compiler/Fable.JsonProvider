namespace Fable.Core

type EmitAttribute(macro: string) =
    inherit System.Attribute()

namespace Fable

module JsonProvider =

    open System.Collections.Generic
    open System.IO
    open System.Net
    open System.Net.Http
    open System.Text.RegularExpressions
    open FSharp.Quotations
    open FSharp.Core.CompilerServices
    open ProviderImplementation.ProvidedTypes
    open System.Text.Json
    open System.Text.Json.Nodes

    open ProviderDsl
    open Fable.Core

    [<Emit("JSON.parse($0)")>]
    let jsonParse (json: string) = obj()

    [<Emit("$0[$1]")>]
    let getProp (o: obj) (k: string) = obj()

    let fetchUrlAsync (url: string) =
        task {
            use handler = new HttpClientHandler(AutomaticDecompression=(DecompressionMethods.GZip ||| DecompressionMethods.Deflate))
            use client = new HttpClient(handler)
            let! resp = client.GetAsync(url)
            use! stream = resp.Content.ReadAsStreamAsync()
            use reader = new StreamReader(stream)
            return reader.ReadToEnd()
        }

    let firstToUpper (s: string) =
        s.[0..0].ToUpper() + s.[1..]

    let getterCode name =
        fun (args: Expr list) -> <@@ getProp %%args.Head name @@>

    let rec makeType typeName (json: JsonNode) =
        match json with
        | :? JsonArray as items ->
            match Seq.tryHead items with
            | None -> Array Any
            // TODO: Check if all items have same type
            | Some item -> makeType typeName item |> Array
        | :? JsonObject as o ->
            let members = o |> Seq.collect (makeMember typeName) |> Seq.toList
            makeCustomType(typeName, members) |> Custom
        | :? JsonValue as v ->
            match v.TryGetValue<JsonElement>() with
            | false, _ -> Any
            | true, el ->
                match el.ValueKind with
                | JsonValueKind.True | JsonValueKind.False -> Bool
                | JsonValueKind.Number -> Float
                | JsonValueKind.String -> String
                //| JTokenType.Null -> Any
                | _ -> Any
        | _ -> Any

    and makeMember ns (prop: KeyValuePair<string, JsonNode>) =
        let name = prop.Key
        let t = makeType (firstToUpper name) prop.Value
        let m = Property(name, t, false, getterCode name)
        let rec makeMember' = function
            | Custom t' -> [ChildType t'; m]
            | Array t' -> makeMember' t'
            | _ -> [m]
        makeMember' t

    let parseJson asm ns typeName (sample: string) =
        let makeRootType withCons basicMembers =
            makeRootType(asm, ns, typeName, [
                yield! basicMembers |> Seq.collect (makeMember "")
                if withCons then
                    yield Constructor(["json", String], fun args -> <@@ jsonParse %%args.Head @@>)
            ])
        try
            match JsonNode.Parse sample with
            | :? JsonObject as o ->
                makeRootType true o |> Ok
            | :? JsonArray as ar ->
                match Seq.tryHead ar with
                | None -> Error "Empty array"
                | Some(:? JsonObject as o) ->
                    let t = makeRootType false o
                    let array = t.MakeArrayType() |> Custom
                    [Method("ParseArray", ["json", String], array, true, fun args -> <@@ jsonParse %%args.Head @@>)]
                    |> addMembers t
                    Ok t
                | _ -> Error "JSON array doesn't contain an object"
            | _ -> Error "Expecting a JSON object or an array containing an object"
        with e ->
            Error e.Message

    [<TypeProvider>]
    type public JsonProvider (config : TypeProviderConfig) as this =
        inherit TypeProviderForNamespaces (config)
        let asm = System.Reflection.Assembly.GetExecutingAssembly()
        let ns = "Fable.JsonProvider"

        let staticParams = [ProvidedStaticParameter("sample",typeof<string>)]
        let generator = ProvidedTypeDefinition(asm, ns, "Generator", Some typeof<obj>, isErased = true)

        do
        try
            generator.DefineStaticParameters(
                parameters = staticParams,
                instantiationFunction = (fun typeName pVals ->
                        match pVals with
                        | [| :? string as arg|] ->
                            let arg = arg.Trim()
                            if Regex.IsMatch(arg, "^https?://") then
                                let res = fetchUrlAsync(arg).Result
                                match parseJson asm ns typeName res with
                                | Ok t -> t
                                | Error e -> failwith $"Response from URL %s{arg} is not a valid JSON (%s{e}): %s{res}"
                            else
                                let content =
                                    // Check if the string is a JSON literal
                                    if arg.StartsWith("{") || arg.StartsWith("[") then arg
                                    else
                                        let filepath =
                                            if Path.IsPathRooted arg then arg
                                            else
                                                Path.GetFullPath(Path.Combine(config.ResolutionFolder, arg))
                                        File.ReadAllText(filepath,System.Text.Encoding.UTF8)

                                match parseJson asm ns typeName content with
                                | Ok t -> t
                                | Error e -> failwith $"Local sample is not a valid JSON (%s{e})"
                        | _ -> failwith "unexpected parameter values"
                    )
                )
            this.AddNamespace(ns, [generator])
        with e ->
            printfn "%s" e.Message

    [<assembly:TypeProviderAssembly>]
    do ()