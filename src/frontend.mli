module type S = sig
  val launch : unit -> unit Lwt.t
end

module Make : functor
  (Backend : Backend.S)
  (ResourceHandler : Front.Handler.S)
  (ArgParser : Front.Cli.S)
  -> S
