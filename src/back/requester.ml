module G = Gemini

module type S = sig
  val init : Gemini.Request.t -> Lwt_ssl.socket Lwt.t
  val close : Lwt_ssl.socket -> unit Lwt.t
  val fetch_header : Lwt_ssl.socket -> string -> string Lwt.t
  val parse_body : Lwt_ssl.socket -> string Lwt.t
end

module Default : S = struct
  open Lwt.Syntax

  let init req =
    let* () = Lib.Log.debug "Creating TLS context" in
    let ctx = Ssl.create_context TLSv1_2 Client_context in
    let* () =
      if req.Gemini.Request.cert <> "" then (
        let* () = Lib.Log.debug "Using client certificate" in
        let cert_re =
          Str.regexp
            "\\(-----BEGIN CERTIFICATE-----.+-----END CERTIFICATE-----\\) \
             \\(-----BEGIN PRIVATE KEY-----.+-----END PRIVATE KEY-----\\)"
        in
        Ssl.use_certificate_from_string ctx
          (Str.replace_first cert_re "\\1" req.cert)
          (Str.replace_first cert_re "\\2" req.cert);
        Lwt.return_unit)
      else Lwt.return_unit
    in
    let* () = Lib.Log.debug "Creating socket" in
    let socket =
      Lwt_unix.socket (Unix.domain_of_sockaddr req.addr.ai_addr) SOCK_STREAM 0
    in
    let* () = Lib.Log.debug "Connecting UNIX socket to address" in
    let* () = Lwt_unix.connect socket req.addr.ai_addr in
    let* () =
      Lib.Log.debug "Embedding UNIX socket using context into TLS socket"
    in
    let ssl = Lwt_ssl.embed_uninitialized_socket socket ctx in
    let* () = Lib.Log.debugf "SNI extension (%s)" req.host in
    Ssl.set_client_SNI_hostname
      (Lwt_ssl.ssl_socket_of_uninitialized_socket ssl)
      req.host;
    let* () = Lib.Log.debug "Connecting to socket" in
    Lwt_ssl.ssl_perform_handshake ssl

  let close socket = Lwt_ssl.close socket

  let input_char ssl =
    let tmp = Lwt_bytes.create 1 in
    let* chr = Lwt_ssl.read_bytes ssl tmp 0 1 in
    if chr = 1 then Lwt.return @@ Lwt_bytes.get tmp 0 else Lwt.fail End_of_file

  let fetch_header socket req =
    let bytes = String.to_bytes req in
    let* _ = Lwt_ssl.write socket bytes 0 @@ Bytes.length bytes in
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
      try%lwt
        let* chr = input_char socket in
        Buffer.add_char buf chr;
        input_in ()
      with
      | Ssl.Read_error Error_zero_return | End_of_file ->
          Lwt.return @@ Buffer.contents buf
      | Ssl.Read_error Error_ssl ->
          let* () = Lib.Log.warn "SSL error, some data may be missing" in
          Lwt.return @@ Buffer.contents buf
    in
    input_in ()
end
