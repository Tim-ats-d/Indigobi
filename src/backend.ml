open Import

module type S = sig
  val get :
    url:string ->
    host:string ->
    port:int ->
    cert:string ->
    (Mime.t * string, [> Err.back | G.Status.err ]) result
end

module Make (Prompt : Prompt.S) (Requester : Requester.S) : S = struct
  let rec request req =
    let socket = Requester.init req in
    match
      G.Request.to_string req |> Requester.fetch_header socket |> G.Header.parse
    with
    | Error (`MalformedHeader | `TooLongHeader) ->
        Error `MalformedServerResponse
    | Error ((`GracefulFail | `InvalidStatusCode _) as err) -> Error err
    | Ok { status; meta } -> (
        match status with
        | `Input (meta, `Sensitive s) ->
            let input =
              Lwt_main.run
              @@ if s then Prompt.prompt_sensitive meta else Prompt.prompt meta
            in
            request @@ G.Request.attach_input req input
        | `Success ->
            let body = Requester.parse_body socket in
            Requester.close socket;
            Ok (Mime.parse meta, body)
        | `Redirect (meta, _) ->
            get
              ~url:Urllib.(to_string @@ parse meta req.host)
              ~host:req.host ~port:req.port ~cert:req.cert
        | ( `TemporaryFailure _ | `PermanentFailure _
          | `ClientCertificateRequired _ ) as err ->
            Error err)

  and get ~url ~host ~port ~cert =
    Ssl.init ();
    match Unix.getaddrinfo host (Int.to_string port) [] with
    | [] -> Error `UnknownHostOrServiceName
    | address ->
        List.fold_left
          (fun acc addr ->
            match acc with
            | Ok _ as ok -> ok
            | Error `NotFound -> (
                try
                  let cert_str =
                    if cert = "" then ""
                    else
                      let ch = open_in cert in
                      let s = really_input_string ch @@ in_channel_length ch in
                      close_in ch;
                      s
                  in
                  match
                    G.Request.create url ~addr ~host ~port ~cert:cert_str
                  with
                  | None -> Error `MalformedLink
                  | Some r -> request r
                with Unix.Unix_error _ -> Error `NotFound)
            | Error _ as err -> err)
          (Error `NotFound) address
end
