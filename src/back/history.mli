module type PATH = sig
  val path : string
end

module type ABSTRACT_HIST = sig
  type entry

  val get_all : unit -> entry list
  val push : entry -> unit
  val search_from_regex : string -> entry list
  val del_from_regex : string -> unit

  include Common.Types.SHOWABLE with type t := entry list
end

module type S = ABSTRACT_HIST with type entry := string

module Make : functor (Path : PATH) -> S
