module type S = sig
  val stylize_gemini :
    ctx:'a Context.t -> Gemini.Gemtext.line -> LTerm_text.t Lwt.t

  val stylize_prompt : string -> LTerm_text.t
  val stylize_warning : string -> LTerm_text.t
  val stylize_error : string -> LTerm_text.t
end

module Make (Theme : Config.Theme.S) : S
