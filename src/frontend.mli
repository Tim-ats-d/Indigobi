module type S = sig
  val search_and_display : default_url:string -> unit Lwt.t
end

module Make : functor
  (Cli : Component.Cli.S)
  (Backend : Backend.S)
  (Args : Component.Args.S)
  -> S
