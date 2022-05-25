module G = Gemini

module type S = sig
  val init : G.Request.t -> Lwt_ssl.socket Lwt.t
  val close : Lwt_ssl.socket -> unit Lwt.t
  val fetch_header : Lwt_ssl.socket -> string -> string Lwt.t
  val parse_body : Lwt_ssl.socket -> string Lwt.t
end

module Default : S = struct
  open Lwt.Syntax

  let init req =
    let ctx = Ssl.create_context TLSv1_2 Client_context in
    (if req.G.Request.cert <> "" then
     let cert_re =
       Str.regexp
         "\\(-----BEGIN CERTIFICATE-----.+-----END CERTIFICATE-----\\) \
          \\(-----BEGIN PRIVATE KEY-----.+-----END PRIVATE KEY-----\\)"
     in
     Ssl.use_certificate_from_string ctx
       (Str.replace_first cert_re "\\1" req.G.Request.cert)
       (Str.replace_first cert_re "\\2" req.G.Request.cert));
    let socket =
      Lwt_unix.socket
        (Unix.domain_of_sockaddr req.G.Request.addr.ai_addr)
        SOCK_STREAM 0
    in
    let* () = Lwt_unix.connect socket req.G.Request.addr.ai_addr in
    let* ssl = Lwt_ssl.ssl_connect socket ctx in
    Ssl.set_client_SNI_hostname
      (Option.get @@ Lwt_ssl.ssl_socket ssl)
      req.G.Request.host;
    Lwt.return ssl

  let close socket = Lwt_ssl.close socket

  let input_char ssl =
    let tmp = Lwt_bytes.create 1 in
    let* chr = Lwt_ssl.read_bytes ssl tmp 0 1 in
    if chr <> 1 then raise End_of_file else Lwt.return @@ Lwt_bytes.get tmp 0

  let fetch_header socket req =
    let bytes = String.to_bytes req in
    let* _ = Lwt_ssl.write socket bytes 0 (Bytes.length bytes) in
    let buf = Buffer.create 4 in
    let* chr = input_char socket in
    Buffer.add_char buf chr;
    let* chr = input_char socket in
    Buffer.add_char buf chr;
    let rec input_in () =
      if String.equal Buffer.(sub buf (length buf - 2) 2) "\r\n" then
        Lwt.return @@ Buffer.contents buf
      else
        let* chr = input_char socket in
        Buffer.add_char buf chr;
        input_in ()
    in
    input_in ()

  let parse_body socket =
    let buf = Buffer.create 512 in
    let rec input_in () =
      Lwt.catch
        (fun () ->
          let* chr = input_char socket in
          Buffer.add_char buf chr;
          input_in ())
        (function
          | Ssl.Read_error Error_zero_return | End_of_file ->
              Lwt.return @@ Buffer.contents buf
          | exn -> Lwt.fail exn)
    in
    input_in ()
end
