type t = { mutable address : string option; mutable raw : bool }

module type S = sig
  val parse : unit -> t
end

module Default : S
