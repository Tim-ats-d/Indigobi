module type S = sig
  val init : Gemini.Request.t -> Lwt_ssl.socket Lwt.t
  val close : Lwt_ssl.socket -> unit
  val fetch_header : Lwt_ssl.socket -> string -> string Lwt.t
  val parse_body : Lwt_ssl.socket -> string Lwt.t
end

module Default : S
