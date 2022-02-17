open Gemini

module type S = sig
  val init : Gemini.GRequest.t -> Ssl.socket
  val close : Ssl.socket -> unit
  val get_header : Ssl.socket -> string -> string
  val get_body : Ssl.socket -> string
end

module Default : S = struct
  let init req =
    let ctx = Ssl.create_context TLSv1_2 Client_context in
    Ssl.open_connection_with_context ctx req.GRequest.addr.ai_addr

  let close = Ssl.shutdown_connection

  let get_header socket req =
    Ssl.output_string socket req;
    let buf = Bytes.create 1029 in
    let (_ : int) = Ssl.read socket buf 0 @@ Bytes.length buf in
    Bytes.to_string buf

  let get_body socket =
    let buf = Bytes.create 10000 in
    let (_ : int) = Ssl.read socket buf 0 @@ Bytes.length buf in
    Bytes.to_string buf
end
