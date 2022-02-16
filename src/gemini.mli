module GRequest : sig
  type t = private { uri : string; addr : Unix.addr_info }

  val create : addr:Unix.addr_info -> string -> t option
  val to_string : t -> string
end

module GStatus : sig
  type t = string
end

module GHeader : sig
  type t = { status : string; mime : string; meta : string }

  val parse : string -> t
end
