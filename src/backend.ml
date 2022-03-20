open Import

module type S = sig
  val get :
    url:string ->
    host:string ->
    (Mime.t * string, [> Common.Err.t | G.Status.err ]) result
end

module Make (Input : Input.S) (Requester : Requester.S) : S = struct
  let rec request req =
    let socket = Requester.init req in
    let hopt =
      G.Request.to_string req |> Requester.fetch_header socket |> G.Header.parse
    in
    match hopt with
    | None -> Error `MalformedServerResponse
    | Some header -> (
        match header.status with
        | `Input (meta, `Sensitive s) ->
            let input =
              Lwt_main.run
              @@ if s then Input.sensitive meta else Input.input meta
            in
            request @@ G.Request.attach_input req input
        | `Success ->
            let body = Requester.parse_body socket in
            Requester.close socket;
            Ok (Mime.parse header.meta, body)
        | `Redirect _ -> failwith "todo: redirection"
        | ( `TemporaryFailure _ | `PermanentFailure _
          | `ClientCertificateRequired _ ) as err ->
            Error err)

  let get ~url ~host =
    Ssl.init ();
    match Unix.getaddrinfo host "1965" [] with
    | [] -> Error `UnknownHostOrServiceName
    | address ->
        List.fold_left
          (fun acc addr ->
            match acc with
            | Ok _ as ok -> ok
            | Error `NotFound -> (
                try
                  match G.Request.create url ~addr with
                  | None -> Error `MalformedLink
                  | Some r -> request r
                with Unix.Unix_error _ -> Error `NotFound)
            | Error _ as err -> err)
          (Error `NotFound)
          address
end
