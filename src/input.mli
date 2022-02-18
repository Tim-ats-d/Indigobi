module type S = sig
  val input : unit -> string
  val sensitive : unit -> string
  val normalize : string -> string
end

module Default : S
