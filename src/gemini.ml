module GRequest = struct
  type t = { uri : string; addr : Unix.addr_info }

  let create ~addr url =
    if Bytes.(length @@ of_string url) > 1024 then None
    else Some { uri = url ^ "\r\n"; addr }

  let to_string { uri; _ } = uri
end

module GStatus = struct
  type err =
    [ `TemporaryFailure of
      string
      * [ `None | `ServerUnavailable | `CgiError | `ProxyError | `SlowDown ]
    | `PermanentFailure of
      string * [ `None | `NotFound | `Gone | `ProxyRequestRefused | `BadRequest ]
    | `ClientCertificateRequired of
      string * [ `None | `CertificateNotAuthorised | `CertificateNotValid ] ]

  type t =
    [ `Input of [ `Sensitive of bool ]
    | `Success
    | `Redirect of [ `Temporary | `Permanent ]
    | err ]

  let of_int meta = function
    | 10 -> `Input (`Sensitive true)
    | 11 -> `Input (`Sensitive false)
    | 20 -> `Success
    | 30 -> `Redirect `Temporary
    | 31 -> `Redirect `Permanent
    | 40 -> `TemporaryFailure (meta, `None)
    | 41 -> `TemporaryFailure (meta, `ServerUnavailable)
    | 42 -> `TemporaryFailure (meta, `CgiError)
    | 43 -> `TemporaryFailure (meta, `ProxyError)
    | 44 -> `TemporaryFailure (meta, `SlowDown)
    | 50 -> `PermanentFailure (meta, `None)
    | 51 -> `PermanentFailure (meta, `NotFound)
    | 52 -> `PermanentFailure (meta, `Gone)
    | 53 -> `PermanentFailure (meta, `ProxyRequestRefused)
    | 59 -> `PermanentFailure (meta, `BadRequest)
    | 60 -> `ClientCertificateRequired (meta, `None)
    | 61 -> `ClientCertificateRequired (meta, `CertificateNotAuthorised)
    | 62 -> `ClientCertificateRequired (meta, `CertificateNotValid)
    | _ -> raise @@ Invalid_argument "GStatus.of_int"

  let show =
    let open Printf in
    function
    | `TemporaryFailure (m, err) ->
        sprintf "Temporary failure: %s%s"
          (match err with
          | `None -> ""
          | `ServerUnavailable -> "server unavailable: "
          | `CgiError -> "CGI error: "
          | `ProxyError -> "proxy error: "
          | `SlowDown -> "slow down: ")
          m
    | `PermanentFailure (m, err) ->
        sprintf "Permanent failure: %s%s"
          (match err with
          | `None -> ""
          | `NotFound -> "not found: "
          | `Gone -> "gone: "
          | `ProxyRequestRefused -> "proxy request refused: "
          | `BadRequest -> "bad request: ")
          m
    | `ClientCertificateRequired (m, err) ->
        sprintf "Client certificate required: %s%s"
          (match err with
          | `None -> ""
          | `BadRequest -> "bad request: "
          | `CertificateNotAuthorised -> "cerficate not authorised: "
          | `CertificateNotValid -> "cerficate not valid: ")
          m
    | _ -> raise @@ Invalid_argument "GStatus.show"
end

module GHeader = struct
  type t = { status : GStatus.t; meta : string }

  let parse str =
    let re = Str.regexp "\\([0-9][0-9]\\) \\(.*\\)\r\n" in
    if Str.string_match re str 0 then
      let meta = Str.matched_group 2 str in
      if Bytes.(length @@ of_string meta) > 1024 then None
      else
        let status =
          Str.matched_group 1 str |> int_of_string |> GStatus.of_int meta
        in
        Some { status; meta }
    else None
end
