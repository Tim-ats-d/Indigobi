module type S = sig
  val search_and_display : url:string -> host:string -> unit Lwt.t
end

module Make : functor (Cli : Component.Cli.S) (Backend : Backend.S) -> S
