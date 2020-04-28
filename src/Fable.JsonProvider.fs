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
        let rec makeMember' = function
            | Custom t' -> [ChildType t'; m]
            | Array t' -> makeMember' t'
            | _ -> [m]
        makeMember' t

    let parseJson asm ns typeName sample =
        let makeRootType withCons basicMembers =
            makeRootType(asm, ns, typeName, [
                yield! basicMembers |> List.collect (makeMember "")
                if withCons then
                    yield Constructor(["json", String], fun args -> <@@ jsonParse %%args.Head @@>)
            ])
        match JsonParser.parse sample with
        | Some(JsonParser.Object members) ->
            makeRootType true members |> Some
        | Some(JsonParser.Array((JsonParser.Object members)::_)) ->
            let t = makeRootType false members
            let array = t.MakeArrayType() |> Custom
            [Method("ParseArray", ["json", String], array, true, fun args -> <@@ jsonParse %%args.Head @@>)]
            |> addMembers t
            Some t
        | _ -> None

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
                                    | Some t -> t
                                    | None -> failwithf "Response from URL %s is not a valid JSON: %s" arg res
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
                            | Some t -> t
                            | None -> failwithf "Local sample is not a valid JSON"
                    | _ -> failwith "unexpected parameter values"
                )
            )

        do this.AddNamespace(ns, [generator])

    [<assembly:TypeProviderAssembly>]
    do ()