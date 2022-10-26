(** Parse Gemtext format. *)

(** {1 Types} *)

type t = line list

and line =
  | Text of string
  | Link of { url : string; name : string option }
  | Preformat of preformat
  | Heading of [ `H1 | `H2 | `H3 ] * string
  | ListItem of string
  | Quote of string

and preformat = { alt : string option; text : string }

(** {2 API} *)

val parse : string -> t
