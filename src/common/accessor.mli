module type S = sig
  val read : unit -> Sexplib.Sexp.t option Lwt.t
  val write : string -> unit Lwt.t
end

module Location : sig
  type t = Cache | Config
end

val make : Location.t -> string -> (module S)
