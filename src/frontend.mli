module type S = sig
  val launch : unit -> unit Lwt.t
end

module Make : functor
  (Backend : Backend.S)
  (ResourceHandler : Component.Handler.S)
  (ArgParser : Component.Cli.S)
  -> S
