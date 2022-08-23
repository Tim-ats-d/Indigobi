type t = { host : string; fingerprint : string; expiration_date : float }

val t_of_sexp : Sexplib.Type.t -> t
val sexp_of_t : t -> Sexplib.Type.t
val cache : (module Common.Accessor.S)
val get_by_host : string -> t option Lwt.t
val save_entry : t -> unit Lwt.t
