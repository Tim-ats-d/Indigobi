module type S = sig
  val handle_text : ?typ:string -> string -> unit Lwt.t
  val handle_gemini : 'a Context.t -> Gemini.Gemtext.t -> unit Lwt.t
  val handle_other : string -> mime:string -> unit Lwt.t

  val handle_err :
    [< `CommonErr of Common.Err.t | `GeminiErr of Gemini.Status.err ] ->
    unit Lwt.t
end

module Make : functor (Printer : Printer.S) (ExternalHandler : ExtHandler.S) ->
  S
