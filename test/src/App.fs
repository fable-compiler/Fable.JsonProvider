module App

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fable.SimpleHttp

let [<Literal>] REMOTE_JSON_URL = "https://jsonplaceholder.typicode.com/todos/10"

let [<Literal>] LOCAL_JSON_SAMPLE = """{
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

type LocalJson = Fable.JsonProvider.Generator<LOCAL_JSON_SAMPLE>
type RemoteJson = Fable.JsonProvider.Generator<REMOTE_JSON_URL>

type Model =
  { LocalJson: string; ParsedLocalJson: LocalJson; RemoteJson: RemoteJson option }

type Msg =
  | LocalJsonUpdated of string
  | RemoteJsonLoaded of RemoteJson option

let download url = async {
    let! (_, res) = Http.get url
    return RemoteJson res
}

let init() : Model * Cmd<Msg> =
  let json = LOCAL_JSON_SAMPLE
  let cmd = Cmd.OfAsync.either download REMOTE_JSON_URL (Some >> RemoteJsonLoaded) (fun _ -> RemoteJsonLoaded None)
  { LocalJson = json; ParsedLocalJson = LocalJson(json); RemoteJson = None }, cmd

let update (msg:Msg) (model:Model) =
    match msg with
    | LocalJsonUpdated json ->
        try
            let parsed = LocalJson json
            { model with LocalJson = json; ParsedLocalJson = parsed }, Cmd.none
        with _ ->
            { model with LocalJson = json }, Cmd.none
    | RemoteJsonLoaded json ->
        { model with RemoteJson = json }, Cmd.none

let view (model:Model) dispatch =
  let par label txt =
    p [] [strong [] [str (label + ": ")]; str txt]
  div [] [
      div []
          [ yield h2 [] [str "Remote JSON"]
            match model.RemoteJson with
            | None -> ()
            | Some json ->
                yield par "Id" (string json.id)
                yield par "UserId" (string json.userId)
                yield par "Title" json.title
                yield par "Completed" (string json.completed)
          ]
      div []
          [ h2 [] [str "Local JSON"]
            par "Window Title" model.ParsedLocalJson.widget.window.title
            par "Image Source" model.ParsedLocalJson.widget.image.src
            par "Text Size" (sprintf "%.2f" model.ParsedLocalJson.widget.text.foo)
            textarea [OnChange (fun ev -> LocalJsonUpdated ev.Value |> dispatch)
                      Style [Width "600px"; Height "600px"]
                     ]
                     [str model.LocalJson] ]
  ]

// App
Program.mkProgram init update view
|> Program.withReactSynchronous "elmish-app"
|> Program.withConsoleTrace
|> Program.run
