module type S = sig
  type socket

  val init : Gemini.Request.t -> (socket, Common.Err.socket_error) Lwt_result.t
  val close : socket -> unit Lwt.t
  val fetch_header : socket -> string -> string Lwt.t
  val parse_body : socket -> string Lwt.t
end

module Default : S
