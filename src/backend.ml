open Import

module type S = sig
  val get :
    url:string ->
    host:string ->
    port:int ->
    cert:string ->
    timeout:float ->
    (Mime.t * string, [> Err.back | Gemini.Status.err ]) Lwt_result.t
end

module Make (Prompt : Prompt.S) (Requester : Requester.S) : S = struct
  open Lwt.Syntax

  type 'a ssl_cert_verification_result =
    | Invalid_certificate of ([> Err.ssl_cert_error | Gemini.Status.err ] as 'a)
    | Valid_certificate

  let cn_re = Str.regexp {|/CN=\(.+\)|}

  let ssl_cert_verification host cert =
    let tofu_cache = Accessor.make Cache "known_hosts" in
    let* tofu_entry = Tofu.get_by_host tofu_cache host in

    let cert_cn = Str.replace_first cn_re "\\1" @@ Ssl.get_subject cert in

    let expiration_date = Ssl.get_expiration_date cert in
    let today = Unix.gmtime @@ Unix.time () in

    if cert_cn <> host then
      if%lwt
        Prompt.prompt_bool
        @@ Printf.sprintf "Mismatched domain names (%s, %s), trust anyway?" host
             cert_cn
      then Lwt.return Valid_certificate
      else
        Lwt.return
        @@ Invalid_certificate (`MismatchedDomainNames (host, cert_cn))
    else if
      expiration_date.tm_year > today.tm_year
      && expiration_date.tm_yday > today.tm_yday
    then
      if%lwt Prompt.prompt_bool "Expired certificate, trust anyway?" then
        Lwt.return Valid_certificate
      else Lwt.return @@ Invalid_certificate `ExpiredCertificate
    else
      let new_entry =
        {
          Tofu.host;
          hash = Ssl.digest `SHA256 cert;
          expiration_date = (expiration_date.tm_year, expiration_date.tm_yday);
        }
      in
      if%lwt
        match tofu_entry with
        | None ->
            let* () = Tofu.save_entry tofu_cache new_entry in
            Lwt.return_false
        | Some { hash = e_hash; expiration_date = e_expiration_date; _ } ->
            if
              fst e_expiration_date < today.tm_year
              && snd e_expiration_date < today.tm_yday
            then
              let* () = Tofu.save_entry tofu_cache new_entry in
              Lwt.return_false
            else if String.equal e_hash @@ Ssl.digest `SHA256 cert then
              Lwt.return_false
            else Lwt.return_true
      then
        if%lwt
          Prompt.prompt_bool
            "Unknown certificate, replace the old one with this one?"
        then
          let* () = Tofu.save_entry tofu_cache new_entry in
          Lwt.return Valid_certificate
        else
          if%lwt Prompt.prompt_bool "Trust it temporarily?" then
            Lwt.return Valid_certificate
          else Lwt.return @@ Invalid_certificate `UntrustedCertificate
      else Lwt.return Valid_certificate

  let rec request timeout req =
    let socket = Requester.init req in
    let make_request =
      let* resolved_socket = socket in
      let certificate =
        Ssl.get_certificate @@ Option.get @@ Lwt_ssl.ssl_socket resolved_socket
      in
      let* verification =
        ssl_cert_verification req.Gemini.Request.host certificate
      in

      match verification with
      | Invalid_certificate err -> Lwt_result.fail err
      | Valid_certificate -> (
          let* header =
            Requester.fetch_header resolved_socket
            @@ Gemini.Request.to_string req
          in
          match Gemini.Header.parse header with
          | Error (`MalformedHeader | `TooLongHeader) ->
              Lwt_result.fail `MalformedServerResponse
          | Error (#Common.Err.status_code as err) -> Lwt_result.fail err
          | Ok { status; meta } -> (
              match status with
              | `Input (meta, `Sensitive s) ->
                  let* input =
                    if s then Prompt.prompt_sensitive meta
                    else Prompt.prompt meta
                  in
                  request timeout @@ Gemini.Request.attach_input req input
              | `Success ->
                  let* body = Requester.parse_body resolved_socket in
                  let* () = Requester.close resolved_socket in
                  Lwt_result.ok @@ Lwt.return (Mime.parse meta, body)
              | `Redirect (meta, _) ->
                  get
                    ~url:Lib.Url.(to_string @@ parse meta req.host)
                    ~host:req.host ~port:req.port ~cert:req.cert ~timeout
              | #Gemini.Status.err as err -> Lwt_result.fail err))
    in
    let timeout =
      let* () = Lwt_unix.sleep timeout in
      Lwt_result.fail `Timeout
    in
    Lwt.pick [ timeout; make_request ]

  and get ~url ~host ~port ~cert ~timeout =
    Ssl.init ();
    let* adresses = Lwt_unix.getaddrinfo host (Int.to_string port) [] in
    match adresses with
    | [] -> Lwt_result.fail `UnknownHostOrServiceName
    | address ->
        let f acc addr =
          match acc with
          | Ok _ as ok -> Lwt.return ok
          | Error `NotFound -> (
              try%lwt
                let* cert =
                  if cert = "" then Lwt.return ""
                  else Lwt_io.with_file ~mode:Input cert Lwt_io.read
                in
                match Gemini.Request.create url ~addr ~host ~port ~cert with
                | None -> Lwt_result.fail `MalformedLink
                | Some r -> request timeout r
              with Unix.Unix_error _ -> Lwt_result.fail `NotFound)
          | Error err -> Lwt_result.fail err
        in
        Lwt_list.fold_left_s f (Error `NotFound) address
end
