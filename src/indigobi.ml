open Component
module G = Gemini

module Make (Input : Input.S) (Requester : Requester.S) = struct
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
            let input = if s then Input.sensitive meta else Input.input meta in
            request @@ G.Request.attach_input req input
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
                  match G.Request.create url ~addr with
                  | None -> Error `MalformedLink
                  | Some r -> request r
                with Unix.Unix_error _ -> Error `NotFound)
            | Error _ as err -> err)
          (Error `NotFound)
          address
end

let main () =
  let module M = Make (Input.Default) (Requester.Default) in
  let res =
    M.get ~url:"gemini://geminispace.info/search" ~host:"geminispace.info"
  in
  match res with
  | Ok (mime, body) -> Printf.printf "%s\n%s" mime body
  | Error (#G.Status.err as e) -> Cmdline_ui.Default.print_err @@ `GeminiErr e
  | Error (#Common.Err.t as e) -> Cmdline_ui.Default.print_err @@ `CommonErr e

module Component = Component
module Common = Common
module Gemini = Gemini
