module type S = sig
  val launch : unit -> unit Lwt.t
end

module Make : functor
  (Backend : Backend.S)
  (Printer : Component.Printer.S)
  (ArgParser : Component.Cli.S)
  -> S
