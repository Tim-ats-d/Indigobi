open Gemini

module Make (Input : Input.S) (Requester : Requester.S) = struct
  let rec request req =
    let socket = Requester.init req in
    let hopt =
      GRequest.to_string req |> Requester.fetch_header socket |> GHeader.parse
    in
    match hopt with
    | None -> Error `MalformedServerResponse
    | Some header -> (
        match header.status with
        | `Input (meta, `Sensitive s) ->
            let input = if s then Input.sensitive meta else Input.input meta in
            request @@ GRequest.attach_input req input
        | `Success ->
            let body = Requester.parse_body socket in
            Requester.close socket;
            Ok (header.meta, body)
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
                  match GRequest.create url ~addr with
                  | None -> Error `MalformedLink
                  | Some r -> request r
                with Unix.Unix_error _ -> Error `NotFound)
            | Error _ as err -> err)
          (Error `NotFound)
          address
end

let main () =
  let module M = Make (Input.Default) (Requester.Default) in
  match
    M.get ~url:"gemini://geminispace.info/search" ~host:"geminispace.info"
  with
  | Ok (mime, body) -> Printf.printf "%s\n%s" mime body
  | Error err -> (
      match err with
      | #Gemini.GStatus.err as e -> print_endline @@ Gemini.GStatus.show e
      | #Err.t as e -> print_endline @@ Err.show e)
