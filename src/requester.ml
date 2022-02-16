module type S = sig
  val init : Gemini.GRequest.t -> Ssl.socket
  val close : Ssl.socket -> unit
  val get_header : Ssl.socket -> string -> string
  val get_body : Ssl.socket -> string
end
