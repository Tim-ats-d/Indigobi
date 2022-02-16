module Make (Requester : Requester.S) = struct
  let request req =
    let socket = Requester.init req in
    let header =
      Gemini.(
        GRequest.to_string req |> Requester.get_header socket |> GHeader.parse)
    in
    match header.status with
    | `Input _ -> failwith "todo: input"
    | `Success ->
        let body = Requester.get_body socket in
        Requester.close socket;
        Ok body
    | `Redirect _ -> failwith "todo: redirection"
    | (`TemporaryFailure _ | `PermanentFailure _ | `ClientCertificateRequired _)
      as err ->
        Error err

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
                  match Gemini.GRequest.create url ~addr with
                  | None -> Error `MalformedLink
                  | Some r -> request r
                with Unix.Unix_error _ -> Error `NotFound)
            | Error _ as err -> err)
          (Error `NotFound)
          address
end

module R : Requester.S = struct
  let init req =
    let ctx = Ssl.create_context TLSv1_2 Client_context in
    Ssl.open_connection_with_context ctx req.Gemini.GRequest.addr.ai_addr

  let close = Ssl.shutdown_connection

  let get_header socket req =
    Ssl.output_string socket req;
    let buf = Bytes.create 1024 in
    let (_ : int) = Ssl.read socket buf 0 @@ Bytes.length buf in
    Bytes.to_string buf

  let get_body socket =
    let buf = Bytes.create 10000 in
    let (_ : int) = Ssl.read socket buf 0 @@ Bytes.length buf in
    Bytes.to_string buf
end

let main () =
  let module M = Make (R) in
  match
    M.get ~url:"gemini://gemini.circumlunar.space/news/"
      ~host:"gemini.circumlunar.space"
  with
  | Ok body -> print_endline body
  | Error _ -> print_endline "todo: error"
