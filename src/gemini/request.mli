type t = {
  base_url : string;
  uri : string;
  host : string;
  port : int;
  cert : Tls.Config.own_cert option;
}

val create :
  host:string ->
  port:int ->
  cert:Tls.Config.own_cert option ->
  string ->
  t option

val attach_input : t -> string -> t
val to_string : t -> string
