module type S = sig
  val handle : string -> mime:string -> unit Lwt.t
end

module Make : functor (Prompt : Prompt.S) -> S
module MakeMirage : functor (Prompt : Prompt.S) -> S
