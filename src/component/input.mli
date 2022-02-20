module type S = sig
  val input : string -> string
  val sensitive : string -> string
end

module Make : functor (Cfg : Common.Config.S) -> S
