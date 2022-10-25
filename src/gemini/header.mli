(** Parse Gemini head specification. *)

(** {1 Types} *)

type t = { status : Status.t; meta : string }

(** {2 API} *)

val parse : string -> (t, Common.Err.header) result
