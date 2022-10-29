(** {1 Types} *)

module type S = sig
  val gemini : Common.Theme.t -> Common.Gemtext.line -> Notty.image
end

(** {2 API} *)

module Default : S
