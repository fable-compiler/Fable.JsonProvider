module App

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fable.SimpleHttp

let [<Literal>] FILE_JSON = "test.json"
let [<Literal>] FILE_JSON_PATH = "../public/" + FILE_JSON
let [<Literal>] REMOTE_JSON = "https://jsonplaceholder.typicode.com/todos"
let [<Literal>] LITERAL_JSON = """{
    "widget": {
        "debug": "on",
        "window": {
            "title": "Sample Konfabulator Widget",
            "name": "main_window",
            "width": 500,
            "height": 500
        },
        "image": {
            "src": "Images/Sun.png",
            "name": "sun1",
            "hOffset": 250,
            "vOffset": 250,
            "alignment": "center"
        },
        "text": {
            "data": "Click Here",
            "size": 36,
            "style": "bold",
            "name": "text1",
            "foo": 250,
            "vOffset": 100,
            "alignment": "center",
            "onMouseUp": "sun1.opacity = (sun1.opacity / 100) * 90;"
        }
    }
}"""

type LiteralJson = Fable.JsonProvider.Generator<LITERAL_JSON>
type FileJson = Fable.JsonProvider.Generator<FILE_JSON_PATH>
type RemoteJson = Fable.JsonProvider.Generator<REMOTE_JSON>

type Model =
  { LiteralJson: string
    ParsedLiteralJson: LiteralJson
    ParsedFileJson: FileJson option
    ParsedRemoteJson: RemoteJson[] option
    SelectedIndex: int }

type Msg =
  | LiteralJsonUpdated of string
  | FileJsonLoaded of string option
  | RemoteJsonLoaded of string option
  | IndexUpdated of int

let download url = async {
    let! (_, res) = Http.get url
    return res
}

let init() : Model * Cmd<Msg> =
  let json = LITERAL_JSON
  let cmd1 = Cmd.OfAsync.either download FILE_JSON
                (Some >> FileJsonLoaded)
                (fun _ -> FileJsonLoaded None)
  let cmd2 = Cmd.OfAsync.either download REMOTE_JSON
                (Some >> RemoteJsonLoaded)
                (fun _ -> RemoteJsonLoaded None)
  { LiteralJson = json
    ParsedLiteralJson = LiteralJson(json)
    ParsedFileJson = None
    ParsedRemoteJson = None
    SelectedIndex = 0 }, Cmd.batch [cmd1; cmd2]

let update (msg:Msg) (model:Model) =
    match msg with
    | LiteralJsonUpdated json ->
        try
            let parsed = LiteralJson json
            { model with LiteralJson = json; ParsedLiteralJson = parsed }, Cmd.none
        with _ ->
            { model with LiteralJson = json }, Cmd.none
    | FileJsonLoaded json ->
        { model with ParsedFileJson = Option.map FileJson json }, Cmd.none
    | RemoteJsonLoaded json ->
        { model with ParsedRemoteJson = Option.map RemoteJson.ParseArray json }, Cmd.none
    | IndexUpdated i ->
        { model with SelectedIndex = i }, Cmd.none

let view (model:Model) dispatch =
  let par label txt =
    p [] [strong [] [str (label + ": ")]; str txt]
  div [] [
      div []
          [ yield h2 [] [str "Remote JSON"]
            match model.ParsedRemoteJson with
            | None -> ()
            | Some todos ->
                let todo = todos.[model.SelectedIndex]
                yield select
                        [Value model.SelectedIndex
                         OnChange (fun ev -> int ev.Value |> IndexUpdated |> dispatch)]
                        [for i = 0 to todos.Length - 1 do
                            yield option [Value i] [str (string i)]]
                yield par "Id" (string todo.id)
                yield par "UserId" (string todo.userId)
                yield par "Title" todo.title
                yield par "Completed" (string todo.completed)
          ]
      div []
          [ yield h2 [] [str "File JSON"]
            match model.ParsedFileJson with
            | None -> ()
            | Some json ->
                yield par "Title" json.glossary.title
                yield par "Glossary Term" json.glossary.GlossDiv.GlossList.GlossEntry.GlossTerm
                yield par "ISO" json.glossary.GlossDiv.GlossList.GlossEntry.Abbrev
          ]
      div []
          [ h2 [] [str "Literal JSON"]
            par "Window Title" model.ParsedLiteralJson.widget.window.title
            par "Image Source" model.ParsedLiteralJson.widget.image.src
            par "Text Size" (sprintf "%.2f" model.ParsedLiteralJson.widget.text.foo)
            textarea [OnChange (fun ev -> LiteralJsonUpdated ev.Value |> dispatch)
                      Style [Width "600px"; Height "600px"]
                      Value model.LiteralJson] []
          ]
  ]

// App
Program.mkProgram init update view
|> Program.withReactSynchronous "elmish-app"
|> Program.withConsoleTrace
|> Program.run
