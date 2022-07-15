module type PATH = sig
  val path : string
end

module type S = sig
  val get_all : unit -> string list Lwt.t
  val push : string -> unit Lwt.t
  val search_from_regex : string -> string list Lwt.t
  val del_from_regex : string -> unit Lwt.t

  include Common.Types.PPABLE with type t := string list
end

module Make : functor (Path : PATH) -> S
