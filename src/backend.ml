open Import

module type S = sig
  val get :
    url:string ->
    host:string ->
    port:int ->
    cert:string ->
    (Mime.t * string, [> Err.back | G.Status.err ]) Lwt_result.t
end

module Make (Prompt : Prompt.S) (Requester : Requester.S) : S = struct
  open Lwt.Syntax

  let rec request req =
    let* socket = Requester.init req in
    let* header = Requester.fetch_header socket @@ G.Request.to_string req in
    match G.Header.parse header with
    | Error (`MalformedHeader | `TooLongHeader) ->
        Lwt_result.fail `MalformedServerResponse
    | Error ((`GracefulFail | `InvalidStatusCode _) as err) ->
        Lwt_result.fail err
    | Ok { status; meta } -> (
        match status with
        | `Input (meta, `Sensitive s) ->
            let* input =
              if s then Prompt.prompt_sensitive meta else Prompt.prompt meta
            in
            request @@ G.Request.attach_input req input
        | `Success ->
            let* body = Requester.parse_body socket in
            Requester.close socket;
            Lwt_result.ok @@ Lwt.return (Mime.parse meta, body)
        | `Redirect (meta, _) ->
            get
              ~url:Urllib.(to_string @@ parse meta req.host)
              ~host:req.host ~port:req.port ~cert:req.cert
        | ( `TemporaryFailure _ | `PermanentFailure _
          | `ClientCertificateRequired _ ) as err ->
            Lwt_result.fail err)

  and get ~url ~host ~port ~cert =
    Ssl.init ();
    let* adresses = Lwt_unix.getaddrinfo host (Int.to_string port) [] in
    match adresses with
    | [] -> Lwt_result.fail `UnknownHostOrServiceName
    | address ->
        let f (acc : (Mime.t * string, [> Err.back | G.Status.err ]) result)
            (addr : Unix.addr_info) :
            (Mime.t * string, [> Err.back | G.Status.err ]) Lwt_result.t =
          match acc with
          | Ok _ as ok -> Lwt.return ok
          | Error `NotFound ->
              Lwt.catch
                (fun () ->
                  let* cert_str =
                    if cert = "" then Lwt.return ""
                    else
                      Lwt_io.with_file ~mode:Input cert (fun _ic ->
                          Lwt.return "foo")
                    (* TODO *)
                  in
                  match
                    G.Request.create url ~addr ~host ~port ~cert:cert_str
                  with
                  | None -> Lwt_result.fail `MalformedLink
                  | Some r -> request r)
                (function
                  | Unix.Unix_error _ -> Lwt_result.fail `NotFound
                  | exn -> Lwt.fail exn)
          | Error err ->
              print_endline "ok";
              Lwt_result.fail (err :> [> Err.back | G.Status.err ])
        in
        Lwt_list.fold_left_s f (Error `NotFound) address
end
