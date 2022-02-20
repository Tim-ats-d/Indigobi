module type S = sig
  val error_clr : LTerm_style.color
  val make_prompt : string -> LTerm_text.markup
  val fmt_text : string -> LTerm_text.markup
  val fmt_link : url:string -> name:string option -> LTerm_text.markup
  val fmt_preformat : string -> string option -> LTerm_text.markup
  val fmt_h1 : string -> LTerm_text.markup
  val fmt_h2 : string -> LTerm_text.markup
  val fmt_h3 : string -> LTerm_text.markup
  val fmt_list_item : string -> LTerm_text.markup
  val fmt_quote : string -> LTerm_text.markup
end
