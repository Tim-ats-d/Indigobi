module type PATH = sig
  val path : string
end

module type BASE_HIST = sig
  type entry

  val get_entries : unit -> entry list
  val add_entry : entry -> unit
  val search_from_regex : string -> entry list
  val del_from_regex : string -> unit

  include Common.Types.SHOWABLE with type t := entry list
end

module type S = BASE_HIST with type entry := string

module Make : functor (Path : PATH) -> S
