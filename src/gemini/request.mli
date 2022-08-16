type bypass = { host : bool; expiration : bool; empty : bool }

type t = {
  base_url : string;
  uri : string;
  host : string;
  port : int;
  cert : Tls.Config.own_cert option;
  bypass : bypass;
}

val default_bypass : bypass

val create :
  bypass:bypass ->
  host:string ->
  port:int ->
  cert:Tls.Config.own_cert option ->
  string ->
  t option

val attach_input : t -> string -> t
val to_string : t -> string
