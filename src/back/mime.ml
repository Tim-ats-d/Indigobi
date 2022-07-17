type t = {
  media_type : media_type;
  encoding : [ `Utf8 | `Other of string ];
  lang : [ `None ];
}

and media_type = Gemini | Text of string | Other of string

let mime_re = Str.regexp "text/\\(.*\\)"

let guess = function
  | "text/gemini" -> Gemini
  | s when Str.string_match mime_re s 0 -> Text (Str.matched_group 1 s)
  | o -> Other o

let parse str =
  let media_type =
    if str = "" then Gemini
    else guess @@ List.hd @@ String.split_on_char ';' str
  in
  { media_type; encoding = `Utf8; lang = `None }
