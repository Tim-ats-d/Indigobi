module type S = sig
  type t
  type color
  type markup

  val error_clr : color
  val make_prompt : string -> markup
  val to_markup : t -> markup
end
