(** Basic mime detector. *)

(** {1 Types} *)

type t = Gemini | Text of string | Other of string

(** {2 API} *)

val parse : string -> t
