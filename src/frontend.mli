module type S = sig
  val get : url:string -> host:string -> unit
end

module Make : functor (Cli : Component.Cli.S) (Backend : Backend.S) -> S
