type t = private { base_url : string; uri : string; addr : Unix.addr_info }

val create : addr:Unix.addr_info -> string -> t option
val attach_input : t -> string -> t
val to_string : t -> string
