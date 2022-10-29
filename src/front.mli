(** {1 Types} *)

module type S = sig
  val main : unit -> unit
end

(** {2 API} *)

module Make : functor
  (Backend : Back.S)
  (ArgParser : Frontend.Cli.S)
  (Printer : Frontend.Printer.S)
  -> S
