open Import

module type S = sig
  val get :
    ?bypass:Gemini.Request.bypass ->
    url:string ->
    host:string ->
    port:int ->
    cert:Tls.Config.own_cert option ->
    float ->
    (Mime.t * string, [> Err.back | Gemini.Status.err ]) Lwt_result.t

  val cert_from_file :
    string -> (Tls.Config.own_cert option, [> Common.Err.back ]) Lwt_result.t
end

module Make (Prompt : Prompt.S) (Requester : Requester.S) : S = struct
  open Lwt.Syntax

  let cert_from_file cert =
    if cert = "" then Lwt_result.ok @@ Lwt.return None
    else
      try%lwt
        let* cert_str = Lwt_io.with_file ~mode:Input cert Lwt_io.read in
        let cert_re =
          Str.regexp
            {|\(-----BEGIN CERTIFICATE-----.+-----END CERTIFICATE-----\) \(-----BEGIN PRIVATE KEY-----.+-----END PRIVATE KEY-----\)|}
        in
        match
          Str.replace_first cert_re "\\1" cert_str
          |> Cstruct.of_string |> X509.Certificate.decode_pem_multiple
        with
        | Ok certificate -> (
            match
              Str.replace_first cert_re "\\2" cert_str
              |> Cstruct.of_string |> X509.Private_key.decode_pem
            with
            | Ok priv_key ->
                Lwt_result.ok @@ Lwt.return
                @@ Some (`Single (certificate, priv_key))
            | Error (`Msg e) -> Lwt_result.fail @@ `InvalidClientCertificate e)
        | Error (`Msg e) -> Lwt_result.fail @@ `InvalidClientCertificate e
      with Unix.Unix_error (Unix.ENOENT, "open", _) ->
        Lwt_result.fail @@ `FileNotFound cert

  let rec request timeout req =
    let* socket_r = Requester.init req in
    match socket_r with
    | Ok socket -> (
        let* header =
          Requester.fetch_header socket @@ Gemini.Request.to_string req
        in
        match Gemini.Header.parse header with
        | Error (`MalformedHeader | `TooLongHeader) ->
            Lwt_result.fail `MalformedServerResponse
        | Error (#Common.Err.status_code as err) -> Lwt_result.fail err
        | Ok { status; meta } -> (
            match status with
            | `Input (meta, `Sensitive s) ->
                let* input =
                  if s then Prompt.prompt_sensitive meta else Prompt.prompt meta
                in
                request timeout @@ Gemini.Request.attach_input req input
            | `Success ->
                let* body = Requester.parse_body socket in
                let* () = Requester.close socket in
                Lwt_result.ok @@ Lwt.return (Mime.parse meta, body)
            | `Redirect (meta, _) ->
                get ~bypass:req.bypass
                  ~url:Lib.Url.(to_string @@ parse meta req.host)
                  ~host:req.host ~port:req.port ~cert:req.cert timeout
            | #Gemini.Status.err as err -> Lwt_result.fail err))
    | Error err -> (
        match err with
        | `Tls (`Error (`AuthenticationFailure validation_error)) -> (
            match validation_error with
            | `LeafInvalidName _ ->
                if%lwt
                  Prompt.prompt_bool
                  @@ Printf.sprintf
                       "Domain %s not present in certificate, proceed anyway?"
                       req.host
                then
                  get
                    ~bypass:{ req.bypass with host = true }
                    ~url:req.uri ~host:req.host ~port:req.port ~cert:req.cert
                    timeout
                else Lwt_result.fail @@ `DomainNameNotPresent req.host
            | `LeafCertificateExpired _ ->
                if%lwt Prompt.prompt_bool "Expired certificate, proceed anyway?"
                then
                  get
                    ~bypass:{ req.bypass with expiration = true }
                    ~url:req.uri ~host:req.host ~port:req.port ~cert:req.cert
                    timeout
                else Lwt_result.fail `ExpiredCertificate
            | _ -> Lwt_result.fail @@ `SocketError err)
        | _ -> Lwt_result.fail @@ `SocketError err)

  and get ?(bypass = Gemini.Request.default_bypass) ~url ~host ~port ~cert
      timeout =
    match Gemini.Request.create url ~host ~port ~cert ~bypass with
    | None -> Lwt_result.fail `MalformedLink
    | Some r -> request timeout r
end
