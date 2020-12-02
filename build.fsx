#load "node_modules/fable-publish-utils/PublishUtils.fs"
open PublishUtils

// run "npm test"
match argsLower with
| "start"::_ ->
    run "npm install"
    run "dotnet build src"
    run "dotnet fable watch test/src --run webpack serve --config test/webpack.config.js"
| "publish"::_ ->
    pushNuget "src/Fable.JsonProvider.fsproj" [] doNothing
| _ -> ()
