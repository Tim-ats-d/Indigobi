type t = {
  media_type : media_type;
  encoding : [ `Other of string | `Utf8 ];
  lang : [ `None ];
}

and media_type = Gemini | Text of string | Other of string


val parse : string -> t
