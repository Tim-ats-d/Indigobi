module type S = sig
  val print_text : string -> string -> unit
  val print_gemini : Gemini.Text.t -> unit
  val print_other : string -> string -> unit

  val print_err :
    [< `CommonErr of Common.Err.t | `GeminiErr of Gemini.Status.err ] -> unit
end

module Make : functor
  (Cfg : Common.Config.S
           with type t := Gemini.Text.line
            and type color = LTerm_style.color
            and type markup = LTerm_text.markup)
  -> S
