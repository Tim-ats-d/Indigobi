module type S = sig
  val handle_text : ?typ:string -> string -> unit Lwt.t
  val handle_gemini : Gemini.Text.t -> unit Lwt.t
  val handle_other : string -> string -> unit Lwt.t

  val handle_err :
    [< `CommonErr of Common.Err.t | `GeminiErr of Gemini.Status.err ] ->
    unit Lwt.t
end

module Make : functor
  (Cfg : Common.Config.S
           with type t := Gemini.Text.line
            and type color = LTerm_style.color
            and type markup = LTerm_text.markup)
  -> S
