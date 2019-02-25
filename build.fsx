#r "node_modules/fable-metadata/lib/Fable.Core.dll"
#load "node_modules/fable-publish-utils/PublishUtils.fs"
open PublishUtils

// run "npm test"
match args with
| IgnoreCase "publish"::_ ->
    pushNuget "src/Fable.JsonProvider.fsproj"
| _ -> ()
