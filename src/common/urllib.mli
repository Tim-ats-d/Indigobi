type t = {
  scheme : string;
  domain : string;
  port : int;
  path : string;
  query : string;
}

val encode : string -> string
val parse : string -> string -> t
val to_string : t -> string
