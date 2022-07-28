type t = line list

and line =
  | Text of string
  | Link of { url : string; name : string option }
  | Preformat of preformat
  | Header of header * string
  | ListItem of string
  | Quote of string

and header = H1 | H2 | H3
and preformat = { alt : string option; text : string }

type headers = (header * string) list

val parse : string -> t * headers
