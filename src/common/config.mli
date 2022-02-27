module type S = sig
  type t

  val error_clr : LTerm_style.color
  val make_prompt : string -> LTerm_text.markup
  val to_markup : t -> LTerm_text.markup
end
