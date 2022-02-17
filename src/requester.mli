module type S = sig
  val init : Gemini.GRequest.t -> Ssl.socket
  val close : Ssl.socket -> unit
  val fetch_header : Ssl.socket -> string -> string
  val parse_body : Ssl.socket -> string
end

module Default : S
