(** {1 Types} *)

module type S = sig
  type socket

  val init : Request.t -> (socket, Common.Err.socket_error) Lwt_result.t
  val close : socket -> unit Lwt.t
  val fetch_header : socket -> string -> string Lwt.t
  val parse_body : socket -> string Lwt.t
end

(** {2 API} *)

module Default : S
