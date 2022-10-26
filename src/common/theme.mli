(** Theming facilities. *)

(** {1 Types} *)

type t = {
  text : Notty.attr;
  link : Notty.attr;
  link_name : Notty.attr;
  preformat : Notty.attr;
  h1 : Notty.attr;
  h2 : Notty.attr;
  h3 : Notty.attr;
  item : Notty.attr;
  quote : Notty.attr;
}
