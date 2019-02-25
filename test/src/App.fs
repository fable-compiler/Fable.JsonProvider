module App

open Elmish
open Elmish.React
open Fable.Helpers.React
open Fable.Helpers.React.Props

let [<Literal>] JSON_SAMPLE = """{
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

type MyJson = Fable.JsonProvider.Generator<JSON_SAMPLE>

type Model =
  { Json: string; Parsed: MyJson }

type Msg =
  | UpdateJson of string

let init() : Model =
  let json = JSON_SAMPLE
  { Json = json; Parsed = MyJson(json) }

let update (msg:Msg) (model:Model) =
    match msg with
    | UpdateJson json ->
      try
        let parsed = MyJson json
        { Json = json; Parsed = parsed }
      with _ ->
        { model with Json = json }    

let view (model:Model) dispatch =
  let par label txt =
    p [] [strong [] [str (label + ": ")]; str txt]              
  div []
      [ par "Window Title" model.Parsed.widget.window.title
        par "Image Source" model.Parsed.widget.image.src
        par "Text Size" (sprintf "%.2f" model.Parsed.widget.text.foo)
        textarea [OnChange (fun ev -> UpdateJson ev.Value |> dispatch)
                  Style [Width "600px"; Height "600px"]
                 ]
                 [str model.Json]
      ]

// App
Program.mkSimple init update view
|> Program.withReact "elmish-app"
|> Program.withConsoleTrace
|> Program.run
