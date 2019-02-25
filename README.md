# Fable.JsonProvider

Simple F# JSON Type Provider compatible with Fable.

## Usage

**IMPORTANT**: Requires fable-compiler **2.2.0-beta-011** or higher. Beta releases are pushed with "next" tag, so please install them with: `npm install fable-compiler@next`

> You can find a usage sample in the `test` directory of this repo.

- First, install the package `Fable.JsonProvider` from Nuget.
- Then generate your model using a JSON sample. You can construct instances with other strings at runtime and the compiler will make sure you only access properties according to the original sample.

> Please note there're currently **no runtime checks to validate the JSON**.

```fsharp
type MyJson = Fable.JsonProvider.Generator<"""{
  "foo": 5,
  "bar": ["baz"]
}""">

let json = MyJson("""{"foo": 10, "bar": [] }""")
printfn "%.0f" json.foo // prints 10
```

You will notice the provider is still very limited. For example, you must pass a JSON literal as sample, file paths or URLs are not yet supported. Please contribute to make the type provider a great tool for all Fable users!