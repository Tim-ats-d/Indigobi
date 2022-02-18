module type S = sig
  val input : string -> string
  val sensitive : string -> string
end

module Default : S
