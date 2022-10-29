(** {1 Types} *)

module type S = sig
  val prompt : string -> string Lwt.t
  val prompt_sensitive : string -> string Lwt.t
  val prompt_bool : string -> bool Lwt.t
end

(** {2 API} *)

module Make : functor (Printer : Printer.S) -> S
