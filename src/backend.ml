open Import

module type S = sig
  val get :
    url:string ->
    host:string ->
    port:int ->
    cert:string ->
    (Mime.t * string, [> Err.back | Gemini.Status.err ]) Lwt_result.t
end

module Make (Prompt : Prompt.S) (Requester : Requester.S) : S = struct
  open Lwt.Syntax

  let rec request req =
    let* socket = Requester.init req in
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
            request @@ Gemini.Request.attach_input req input
        | `Success ->
            let* body = Requester.parse_body socket in
            let* () = Requester.close socket in
            Lwt_result.ok @@ Lwt.return (Mime.parse meta, body)
        | `Redirect (meta, _) ->
            get
              ~url:Urllib.(to_string @@ parse meta req.host)
              ~host:req.host ~port:req.port ~cert:req.cert
        | #Gemini.Status.err as err -> Lwt_result.fail err)

  and get ~url ~host ~port ~cert =
    Ssl.init ();
    let* adresses = Lwt_unix.getaddrinfo host (Int.to_string port) [] in
    match adresses with
    | [] -> Lwt_result.fail `UnknownHostOrServiceName
    | address ->
        let f acc addr =
          match acc with
          | Ok _ as ok -> Lwt.return ok
          | Error `NotFound ->
              Lwt.catch
                (fun () ->
                  let* cert =
                    if cert = "" then Lwt.return ""
                    else Lwt_io.with_file ~mode:Input cert Lwt_io.read
                  in
                  match Gemini.Request.create url ~addr ~host ~port ~cert with
                  | None -> Lwt_result.fail `MalformedLink
                  | Some r -> request r)
                (function
                  | Unix.Unix_error _ -> Lwt_result.fail `NotFound
                  | exn -> Lwt.fail exn)
          | Error err -> Lwt_result.fail err
        in
        Lwt_list.fold_left_s f (Error `NotFound) address
end
