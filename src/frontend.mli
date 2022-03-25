module type S = sig
  val launch : unit -> unit Lwt.t
end

module Make : functor
  (Backend : Backend.S)
  (PPrint : Component.Pprint.S)
  (ArgParser : Component.Args.S)
  -> S
