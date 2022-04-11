type t = {
  media_type : [ `Gemini | `Other of string | `Text of string ];
  encoding : [ `Other of string | `Utf8 ];
  lang : [ `None ];
}

val parse : string -> t
