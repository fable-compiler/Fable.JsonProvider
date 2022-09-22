#r "nuget: Fable.PublishUtils, 2.4.0"

open PublishUtils

let args =
    fsi.CommandLineArgs
    |> Array.skip 1
    |> List.ofArray

// run "npm test"
match args with
| IgnoreCase "start"::_ ->
    run "npm install"
    run "dotnet build src"
    run "dotnet fable watch test/src --run webpack serve --config test/webpack.config.js"
| IgnoreCase "publish"::_ ->
    pushFableNuget "src/Fable.JsonProvider.fsproj" [] doNothing
| _ -> ()
