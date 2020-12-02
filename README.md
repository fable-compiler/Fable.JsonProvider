# Fable.JsonProvider

Simple F# JSON Type Provider compatible with Fable. Currently tested with Fable 3 RC 11.

## Testing

```shell
dotnet fsi build.fsx start
```

## Usage

- First, install the package `Fable.JsonProvider` from Nuget.
- Then generate your model using a JSON url, file path or literal string. You can construct instances with other strings at runtime and the compiler will make sure you only access properties according to the original sample.

> Please note there're currently **no runtime checks to validate the JSON**.

```fsharp
type MyJson = Fable.JsonProvider.Generator<"""{
  "foo": 5,
  "bar": ["baz"]
}""">

let json = MyJson("""{"foo": 10, "bar": [] }""")
printfn "%.0f" json.foo // prints 10
```
