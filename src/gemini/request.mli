type t = {
  base_url : string;
  uri : string;
  addr : Unix.addr_info;
  host : string;
  cert : string;
}

val create :
  addr:Unix.addr_info -> host:string -> cert:string -> string -> t option

val attach_input : t -> string -> t
val to_string : t -> string
