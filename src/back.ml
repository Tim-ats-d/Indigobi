open Common
open Backend

module type S = sig
  val get :
    ?bypass:Request.bypass ->
    url:string ->
    host:string ->
    port:int ->
    cert:Tls.Config.own_cert option ->
    float ->
    (Mime.t * string, [> Err.back | Status.err ]) Lwt_result.t

  val cert_from_file :
    string -> (Tls.Config.own_cert option, [> Common.Err.back ]) Lwt_result.t
end

module Make (Prompt : Frontend.Prompt.S) (Requester : Requester.S) : S = struct
  open Lwt.Syntax

  let cert_re =
    Re.compile
      Re.(
        seq
          [
            group
              (seq
                 [
                   str "-----BEGIN CERTIFICATE-----";
                   rep any;
                   str "-----END CERTIFICATE-----";
                 ]);
            char ' ';
            group
              (seq
                 [
                   str "-----BEGIN PRIVATE KEY-----";
                   rep any;
                   str "-----END PRIVATE KEY-----";
                 ]);
          ])

  let cert_from_file cert =
    if cert = "" then Lwt_result.ok Lwt.return_none
    else
      try%lwt
        let* cert_str = Lwt_io.with_file ~mode:Input cert Lwt_io.read in

        match
          Re.replace ~all:false cert_re
            ~f:(fun grp -> Re.Group.get grp 1)
            cert_str
          |> Cstruct.of_string |> X509.Certificate.decode_pem_multiple
        with
        | Ok certificate -> (
            match
              Re.replace ~all:false cert_re
                ~f:(fun grp -> Re.Group.get grp 2)
                cert_str
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
        let* header = Requester.fetch_header socket @@ Request.to_string req in
        match Header.parse header with
        | Error (`MalformedHeader | `TooLongHeader) ->
            Lwt_result.fail `MalformedServerResponse
        | Error (#Common.Err.status_code as err) -> Lwt_result.fail err
        | Ok { status; meta } -> (
            match status with
            | `Input (meta, `Sensitive s) ->
                let* input =
                  if s then Prompt.prompt_sensitive meta else Prompt.prompt meta
                in
                Url.encode input |> Request.attach_input req |> request timeout
            | `Success ->
                let* body = Requester.parse_body socket in
                let* () = Requester.close socket in
                Lwt_result.ok @@ Lwt.return (Mime.parse meta, body)
            | `Redirect (meta, _) ->
                get ~bypass:req.bypass
                  ~url:Url.(to_string @@ parse meta req.host)
                  ~host:req.host ~port:req.port ~cert:req.cert timeout
            | #Status.err as err -> Lwt_result.fail err))
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
            | `InvalidFingerprint (cert, _, _) ->
                if%lwt
                  Prompt.prompt_bool
                    "Untrusted certificate, replace trusted one with this new \
                     one?"
                then
                  let* () =
                    Tofu.save_entry
                      {
                        Tofu.host = req.host;
                        fingerprint =
                          Cstruct.to_string
                          @@ X509.Certificate.fingerprint `SHA256 cert;
                        expiration_date =
                          Ptime.to_float_s @@ snd
                          @@ X509.Certificate.validity cert;
                      }
                  in
                  get ~bypass:req.bypass ~url:req.uri ~host:req.host
                    ~port:req.port ~cert:req.cert timeout
                else
                  if%lwt Prompt.prompt_bool "Proceed anyway?" then
                    get
                      ~bypass:{ req.bypass with fingerprint = true }
                      ~url:req.uri ~host:req.host ~port:req.port ~cert:req.cert
                      timeout
                  else Lwt_result.fail `UntrustedCertificate
            | _ -> Lwt_result.fail @@ `SocketError err)
        | _ -> Lwt_result.fail @@ `SocketError err)

  and get ?(bypass = Request.default_bypass) ~url ~host ~port ~cert timeout =
    match Request.create url ~host ~port ~cert ~bypass with
    | None -> Lwt_result.fail `MalformedLink
    | Some r -> request timeout r
end
