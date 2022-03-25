module type S = sig
  val launch : unit -> unit Lwt.t
end

module Make : functor
  (Cli : Component.Cli.S)
  (Backend : Backend.S)
  (ArgParser : Component.Args.S)
  -> S
