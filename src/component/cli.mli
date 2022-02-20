module type S = sig
  val print_err :
    [< `CommonErr of Common.Err.t | `GeminiErr of Gemini.Status.err ] -> unit
end

module Default : S
