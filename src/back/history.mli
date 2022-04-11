module type PATH = sig
  val path : string
end

module type S = sig
  val add_entry : string -> unit
  val iter_s : (string -> unit Lwt.t) -> unit Lwt.t
  val search_from_regex : string -> string list Lwt.t
end

module Make : functor (Path : PATH) -> S
