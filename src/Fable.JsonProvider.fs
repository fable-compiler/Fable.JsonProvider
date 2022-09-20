namespace Fable.Core

type EmitAttribute(macro: string) =
    inherit System.Attribute()

namespace Fable

module JsonProvider =

    open System
    open System.IO
    open System.Net
    open System.Text.RegularExpressions
    open FSharp.Quotations
    open FSharp.Core.CompilerServices
    open ProviderImplementation.ProvidedTypes
    open Newtonsoft.Json.Linq

    open ProviderDsl
    open Fable.Core

    [<Emit("JSON.parse($0)")>]
    let jsonParse (json: string) = obj()

    [<Emit("$0[$1]")>]
    let getProp (o: obj) (k: string) = obj()

    let fetchUrlAsync (url: string) =
        async {
            let req = WebRequest.CreateHttp(url)
            req.AutomaticDecompression <- DecompressionMethods.GZip ||| DecompressionMethods.Deflate
            use! resp = req.AsyncGetResponse()
            use stream = resp.GetResponseStream()
            use reader = new IO.StreamReader(stream)
            return reader.ReadToEnd()
        }

    let firstToUpper (s: string) =
        s.[0..0].ToUpper() + s.[1..]

    let getterCode name =
        fun (args: Expr list) -> <@@ getProp %%args.Head name @@>

    let rec makeType typeName (json: JToken) =
        match json with
        | :? JArray as items ->
            match Seq.tryHead items with
            | None -> Array Any
            // TODO: Check if all items have same type
            | Some item -> makeType typeName item |> Array
        | :? JObject as o ->
            let members = o.Properties() |> Seq.collect (makeMember typeName)
            makeCustomType(typeName, members) |> Custom
        | :? JValue as v ->
            match v.Type with
            | JTokenType.Boolean -> Bool
            | JTokenType.Integer -> Int
            | JTokenType.Float -> Float
            | JTokenType.String -> String
            //| JTokenType.Null -> Any
            | _ -> Any
        | _ -> Any

    and makeMember ns (prop: JProperty) =
        let name = prop.Name
        let t = makeType (firstToUpper name) prop.Value
        let m = Property(name, t, false, getterCode name)
        let rec makeMember' = function
            | Custom t' -> [ChildType t'; m]
            | Array t' -> makeMember' t'
            | _ -> [m]
        makeMember' t

    let parseJson asm ns typeName sample =
        let makeRootType withCons basicMembers =
            makeRootType(asm, ns, typeName, [
                yield! basicMembers |> Seq.collect (makeMember "")
                if withCons then
                    yield Constructor(["json", String], fun args -> <@@ jsonParse %%args.Head @@>)
            ])
        try
            match JToken.Parse sample with
            | :? JObject as o ->
                o.Properties() |> makeRootType true |> Ok
            | :? JArray as ar ->
                match Seq.tryHead ar with
                | None -> Error "Empty array"
                | Some(:? JObject as o) ->
                    let t = o.Properties() |> makeRootType false
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

        do generator.DefineStaticParameters(
            parameters = staticParams,
            instantiationFunction = (fun typeName pVals ->
                    match pVals with
                    | [| :? string as arg|] ->
                        let arg = arg.Trim()
                        if Regex.IsMatch(arg, "^https?://") then
                            async {
                                let! res = fetchUrlAsync arg
                                return
                                    match parseJson asm ns typeName res with
                                    | Ok t -> t
                                    | Error e -> failwithf "Response from URL %s is not a valid JSON (%s): %s" arg e res
                            } |> Async.RunSynchronously
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
                            | Error e -> failwithf "Local sample is not a valid JSON (%s)" e
                    | _ -> failwith "unexpected parameter values"
                )
            )

        do this.AddNamespace(ns, [generator])

    [<assembly:TypeProviderAssembly>]
    do ()