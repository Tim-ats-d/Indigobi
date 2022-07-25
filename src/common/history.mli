module type ENTRY = sig
  type t

  val equal : t -> t -> bool
  val from_string : string -> t
  val to_string : t -> string
  val show : t -> string
  val sexp_of_t : t -> Sexplib.Type.t
  val t_of_sexp : Sexplib.Type.t -> t
end

type 'a t

val create : fname:string -> (module ENTRY with type t = 'a) -> 'a t
val entry : 'a t -> (module ENTRY with type t = 'a)
val get : 'a t -> 'a list Lwt.t
val push : 'a t -> 'a -> unit Lwt.t
val mem : 'a t -> 'a -> bool Lwt.t
val search_from_regex : 'a t -> string -> 'a list Lwt.t
val del_from_regex : 'a t -> string -> unit Lwt.t
val get_pp_entries : 'a t -> unit -> 'a list -> string
