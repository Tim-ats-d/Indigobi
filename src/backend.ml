open Import

module type S = sig
  val get :
    url:string ->
    host:string ->
    port:int ->
    cert:string ->
    (Mime.t * string, [> Common.Err.t | G.Status.err ]) result
end

module Make (Input : Input.S) (Requester : Requester.S) : S = struct
  let rec request req =
    let socket = Requester.init req in
    match
      G.Request.to_string req |> Requester.fetch_header socket |> G.Header.parse
    with
    | None -> Error `MalformedServerResponse
    | Some { status; meta } -> (
        match status with
        | `Input (meta, `Sensitive s) ->
            let input =
              Lwt_main.run
              @@ if s then Input.sensitive meta else Input.input meta
            in
            request @@ G.Request.attach_input req input
        | `Success ->
            let body = Requester.parse_body socket in
            Ok (Mime.parse meta, body)
        | `Redirect _ -> failwith "todo: redirection"
        | ( `TemporaryFailure _ | `PermanentFailure _
          | `ClientCertificateRequired _ ) as err ->
            Error err)

  let get ~url ~host ~port ~cert =
    Ssl.init ();
    let cert_str =
      if cert <> "" then (
        let ch = open_in cert in
        let s = really_input_string ch (in_channel_length ch) in
        close_in ch;
        s)
      else ""
    in
    match G.Request.create url ~host ~port ~cert:cert_str with
    | None -> Error `MalformedLink
    | Some r -> request r
end
