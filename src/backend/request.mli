type bypass = {
  host : bool;
  expiration : bool;
  empty : bool;
  fingerprint : bool;
}

type t = { uri : Uri.t; cert : Tls.Config.own_cert option; bypass : bypass }

val default_bypass : bypass

val create :
  bypass:bypass -> cert:Tls.Config.own_cert option -> Uri.t -> t option

val attach_input : t -> string -> t
val to_string : t -> string
