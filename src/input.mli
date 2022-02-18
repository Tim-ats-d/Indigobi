module type S = sig
  val input : unit -> string
  val sensitive : unit -> string
end

module Default : S
