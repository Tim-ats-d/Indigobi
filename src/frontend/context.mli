open Common

(** {1 Types} *)

type t = {
  term : Term.t;
  document : document;  (** Document displayed *)
  homepage : document;
  theme : Theme.t;
  mode : mode;  (** Current mode *)
  tab : tab * Mime.t;
  input : string;
  offset : int;  (** Scroll offset *)
  range : int;  (** Scroll range *)
  reload : bool;
  history : tab Zipper.t;  (** Page history *)
  args : Cli.t;  (** CLI arguments supplied *)
}

and document = Gemtext of Gemtext.t | Text of string * string
and mode = Browse | Input
and tab = Home | Page of { address : string }

val make :
  term:Term.t ->
  theme:Theme.t ->
  homepage:document ->
  tab:tab ->
  args:Cli.t ->
  t

(** {2 API} *)

val set_mode : mode -> t -> t
val set_history : tab Zipper.t -> t -> t
val set_home : document -> t -> t
val set_page : document -> string -> t -> t
val set_error : [ Backend.Status.err | Err.err ] -> address:string -> t -> t
val reload : bool -> t -> t
val scroll : t -> [ `Down | `Up ] -> t
val toggle : t -> mode -> default:mode -> t

val delete_last : t -> t
(** Remove the last character of current input. *)

val pp_mode : unit -> mode -> string
