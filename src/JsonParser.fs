// JsonParser by Jon Harrop: https://gist.github.com/jdh30/50741cd6d094004203b1dce019726ebb
module JsonParser

type Json =
  | Null
  | Bool of bool
  | Number of float
  | String of string
  | Array of Json list
  | Object of (string * Json) list

type Bracket = Open | Close

type Token =
  | LITERAL of Json
  | ARRAY of Bracket
  | OBJECT of Bracket

let (|C|_|) (s: string, i) =
  if i < s.Length then Some(s.[i], (s, i+1)) else None

let numeric = set['0'..'9'] + set['+'; '-'; '.'; 'e'; 'E']
let whitespace = set[','; ':'; '\u0009'; '\u000a'; '\u000d'; '\u0020']

let (|Contains|_|) alphabet = function
  | C(c, it) when Set.contains c alphabet -> Some it
  | _ -> None

let rec (|Star|) (|Patt|_|) = function
  | Patt(x, Star (|Patt|_|) (xs, it)) -> x::xs, it
  | it -> [], it

let rec (|LexNumber|_|) = function
  | Contains numeric (LexNumber it | it) -> Some it
  | _ -> None

let rec (|LexString|) = function
  | C('"', it)
  | C('\\', C(('"' | '\\' | '/' | 'b' | 'n' | 'r' | 't'), LexString it))
  | C('\\', C('u', C(_, C(_, C(_, C(_, LexString it))))))
  | C(_, LexString it)
  | it -> it

let rec (|Lex|_|) = function
  | Contains whitespace it -> (|Lex|_|) it
  | C('n', C('u', C('l', C('l', it)))) -> Some(LITERAL Null, it)
  | C('t', C('r', C('u', C('e', it)))) -> Some(LITERAL(Bool true), it)
  | C('f', C('a', C('l', C('s', C('e', it))))) -> Some(LITERAL(Bool false), it)
  | LexNumber((s, last) as it) & (_, first) ->
      Some(LITERAL(Number(float s.[first..last-1])), it)
  | C('"', (LexString(it & (s, last)) & (_, first))) ->
      let s = System.Text.RegularExpressions.Regex.Unescape s.[first..last-2]
      Some(LITERAL(String s), it)
  | C('[', it) -> Some(ARRAY Open, it)
  | C(']', it) -> Some(ARRAY Close, it)
  | C('{', it) -> Some(OBJECT Open, it)
  | C('}', it) -> Some(OBJECT Close, it)
  | _ -> None

let rec (|ParseJSON|_|) = function
  | Lex(LITERAL json, it) -> Some(json, it)
  | Lex(ARRAY Open, Star (|ParseJSON|_|) (jsons, Lex(ARRAY Close, it))) ->
      Some(Array jsons, it)
  | Lex(OBJECT Open, Star (|ParseMember|_|) (members, Lex(OBJECT Close, it))) ->
      Some(Object members, it)
  | _ -> None
and (|ParseMember|_|) = function
  | Lex(LITERAL(String key), ParseJSON(value, it)) -> Some((key, value), it)
  | _ -> None

let parse s = (|ParseJSON|_|) (s, 0) |> Option.map fst