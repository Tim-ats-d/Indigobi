open Common

(** {1 Types} *)

type t = {
  term : Term.t;
  document : document;  (** Document displayed *)
  homepage : document;
  theme : Theme.t;
  mode : mode;  (** Current mode *)
  tab : tab * Mime.t;
  offset : int;  (** Scroll offset *)
  range : int;  (** Scroll range *)
  reload : bool;
  args : Cli.t;  (** CLI arguments supplied *)
}

and document = Gemtext of Gemtext.t | Text of string * string
and mode = Browse
and tab = Home | Page of { address : string }

val make :
  term:Term.t ->
  theme:Theme.t ->
  homepage:document ->
  tab:tab ->
  args:Cli.t ->
  t

(** {2 API} *)

val set_home : document -> t -> t
val set_page : document -> string -> t -> t
val set_error : [ Backend.Status.err | Err.err ] -> t -> t
val reload : bool -> t -> t
val scroll : t -> [ `Down | `Up ] -> t
val pp_mode : unit -> mode -> string
