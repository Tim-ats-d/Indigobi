module type ENTRY = sig
  type t

  include Types.STRINGABLE with type t := t
  include Types.SHOWABLE with type t := t
  include Types.SEXPABLE with type t := t
end

type 'a t

val create : fname:string -> (module ENTRY with type t = 'a) -> 'a t
val get : 'a t -> 'a list Lwt.t
val push : 'a t -> 'a -> unit Lwt.t
val search_from_regex : 'a t -> string -> 'a list Lwt.t
val del_from_regex : 'a t -> string -> unit Lwt.t
val get_pp_entries : 'a t -> unit -> 'a list -> string
