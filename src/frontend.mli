module type S = sig
  val launch : unit -> unit Lwt.t
end

module Make : functor (ArgParser : Front.Cli.S) -> S
