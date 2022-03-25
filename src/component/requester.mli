module type S = sig
  val init : Gemini.Request.t -> Ssl.context
  val fetch_header : Ssl.context -> string -> string
  val parse_body : Ssl.context -> string
end

module Default : S
