module type S = sig
  val prompt : string -> string Lwt.t
  val prompt_sensitive : string -> string Lwt.t
  val prompt_bool : string -> bool Lwt.t
end

module Make : functor (Printer : Printer.S) -> S
