type t = line list

and line =
  | Text of string
  | Link of { url : string; name : string option }
  | Unformat of string
  | Heading of [ `H1 | `H2 | `H3 ]
  | ListItem of string
  | Quote of string
