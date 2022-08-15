type t = {
  base_url : string;
  uri : string;
  host : string;
  port : int;
  cert : string;
}

val create : host:string -> port:int -> cert:string -> string -> t option
val attach_input : t -> string -> t
val to_string : t -> string
