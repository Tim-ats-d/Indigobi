module type S = sig
  val stylize_gemini : Gemini.Text.line -> LTerm_text.t
  val stylize_error : string -> LTerm_text.t
  val stylize_prompt : string -> LTerm_text.t
end

module Make : functor (Theme : Theme.S) -> S
module Theme = Theme
