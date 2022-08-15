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

  let rec request timeout req =
    let* socket_r = Requester.init req in
    match socket_r with
    | Ok socket -> (
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
                request timeout @@ Gemini.Request.attach_input req input
            | `Success ->
                let* body = Requester.parse_body socket in
                let* () = Requester.close socket in
                Lwt_result.ok @@ Lwt.return (Mime.parse meta, body)
            | `Redirect (meta, _) ->
                get
                  ~url:Lib.Url.(to_string @@ parse meta req.host)
                  ~host:req.host ~port:req.port ~cert:req.cert ~timeout
            | #Gemini.Status.err as err -> Lwt_result.fail err))
    | Error err -> Lwt_result.fail err

  and get ~url ~host ~port ~cert ~timeout =
    let* cert =
      if cert = "" then Lwt.return ""
      else Lwt_io.with_file ~mode:Input cert Lwt_io.read
    in
    match Gemini.Request.create url ~host ~port ~cert with
    | None -> Lwt_result.fail `MalformedLink
    | Some r -> request timeout r
end
