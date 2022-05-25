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
  if str = "" then { media_type = Gemini; encoding = `Utf8; lang = `None }
  else
    match String.split_on_char ';' str with
    | [ mtype ] -> { media_type = guess mtype; encoding = `Utf8; lang = `None }
    | _ -> failwith "todo: lang and charset attributes"
