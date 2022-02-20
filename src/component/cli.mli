module type S = sig
  val print_gemini : Gemini.Text.t -> unit

  val print_err :
    [< `CommonErr of Common.Err.t | `GeminiErr of Gemini.Status.err ] -> unit
end

module Make : functor (Cfg : Common.Config.S) -> S
