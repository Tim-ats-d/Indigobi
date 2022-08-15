module G = Gemini

module type S = sig
  type socket

  val init : Gemini.Request.t -> (socket, Tls.Engine.failure) Lwt_result.t
  val close : socket -> unit Lwt.t
  val fetch_header : socket -> string -> string Lwt.t
  val parse_body : socket -> string Lwt.t
end

module Default : S = struct
  open Lwt.Syntax

  type socket = Tls_lwt.ic * Tls_lwt.oc

  let cert_re =
    Str.regexp
      {|\(-----BEGIN CERTIFICATE-----.+-----END CERTIFICATE-----\) \(-----BEGIN PRIVATE KEY-----.+-----END PRIVATE KEY-----\)|}

  let init req =
    try%lwt
      let socket =
        Tls_lwt.connect_ext
          (Tls.Config.client ~authenticator:(fun ?ip:_ ~host:_ _ -> Ok None) ())
          (req.G.Request.host, req.G.Request.port)
      in
      Lwt_result.ok socket
    with Tls_lwt.Tls_failure e -> Lwt_result.fail e

  let close (ic, oc) = Lwt.join [ Lwt_io.close ic; Lwt_io.close oc ]
  let input_char (ic, _) = Lwt_io.read_char ic

  let fetch_header (ic, oc) req =
    let* () = Lwt_io.write oc req in
    let buf = Buffer.create 4 in
    let* chr = input_char (ic, oc) in
    Buffer.add_char buf chr;
    let* chr = input_char (ic, oc) in
    Buffer.add_char buf chr;
    let rec input_in () =
      if String.equal Buffer.(sub buf (length buf - 2) 2) "\r\n" then
        Lwt.return @@ Buffer.contents buf
      else
        let* chr = input_char (ic, oc) in
        Buffer.add_char buf chr;
        input_in ()
    in
    input_in ()

  let parse_body socket =
    let buf = Buffer.create 512 in
    let rec input_in () =
      try%lwt
        let* chr = input_char socket in
        Buffer.add_char buf chr;
        input_in ()
      with _ as err -> raise err
    in
    input_in ()
end
