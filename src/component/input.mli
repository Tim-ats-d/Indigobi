module type S = sig
  val input : string -> string
  val sensitive : string -> string
end

module Make : functor
  (Cfg : Common.Config.S
           with type color = LTerm_style.color
            and type markup = LTerm_text.markup)
  -> S
