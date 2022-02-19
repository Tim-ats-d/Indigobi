type t = line list

and line =
  | Text of string
  | Link of { url : string; name : string option }
  | Preformat of preformat
  | Heading of [ `H1 | `H2 | `H3 ]
  | ListItem of string
  | Quote of string

and preformat = { alt : string option; text : string }

val parse : string -> t
