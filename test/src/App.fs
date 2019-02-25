module App

open Elmish
open Elmish.React
open Fable.Helpers.React
open Fable.Helpers.React.Props

let [<Literal>] JSON_SAMPLE = """{
    "foo": 5,
    "bar": "bar",
    "baz": [1,2,3]
}"""

let [<Literal>] JSON_SAMPLE2 = """{
    "haha": 10
}"""

type MyJson = Fable.JsonProvider.Generator<JSON_SAMPLE>
type MyJson2 = Fable.JsonProvider.Generator<JSON_SAMPLE2>

let json = MyJson(JSON_SAMPLE)
let json2 = MyJson2(JSON_SAMPLE2)

// MODEL

type Model = int

type Msg =
| Increment
| Decrement

let init() : Model = 0

// UPDATE

let update (msg:Msg) (model:Model) =
    match msg with
    | Increment -> model + 1
    | Decrement -> model - 1

// VIEW (rendered with React)

let view (model:Model) dispatch =
  div []
      [ button [ OnClick (fun _ -> dispatch Increment) ] [ str "+" ]
        div [] [ str (string model) ]
        button [ OnClick (fun _ -> dispatch Decrement) ] [ str "-" ]
        p [] [str <| sprintf "%.2f %s %.0f" json.foo json.bar json2.haha]  
      ]

// App
Program.mkSimple init update view
|> Program.withReact "elmish-app"
|> Program.withConsoleTrace
|> Program.run
