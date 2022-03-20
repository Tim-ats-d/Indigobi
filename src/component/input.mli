module type S = sig
  val input : string -> string Lwt.t
  val sensitive : string -> string Lwt.t
end

module Make : functor
  (Cfg : Common.Config.S
           with type color = LTerm_style.color
            and type markup = LTerm_text.markup)
  -> S
