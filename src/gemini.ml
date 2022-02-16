module GRequest = struct
  type t = { uri : string; addr : Unix.addr_info }

  let create ~addr url =
    if Bytes.(length @@ of_string url) > 1024 then None
    else Some { uri = url ^ "\r\n"; addr }

  let to_string { uri; _ } = uri
end

module GStatus = struct
  type t =
    [ `Input of [ `Sensitive of bool ]
    | `Success
    | `Redirect of [ `Temporary | `Permanent ]
    | `TemporaryFailure of
      [ `None | `ServerUnavailable | `CgiError | `ProxyError | `SlowDown ]
    | `PermanentFailure of
      [ `None | `NotFound | `Gone | `ProxyRequestRefused | `BadRequest ]
    | `ClientCertificateRequired of
      [ `None | `CertificateNotAuthorised | `CertificateNotValid ] ]

  let of_int = function
    | 10 -> `Input (`Sensitive true)
    | 11 -> `Input (`Sensitive false)
    | 20 -> `Success
    | 30 -> `Redirect `Temporary
    | 31 -> `Redirect `Permanent
    | 40 -> `TemporaryFailure `None
    | 41 -> `TemporaryFailure `ServerUnavailable
    | 42 -> `TemporaryFailure `CgiError
    | 43 -> `TemporaryFailure `ProxyError
    | 44 -> `TemporaryFailure `SlowDown
    | 50 -> `PermanentFailure `None
    | 51 -> `PermanentFailure `NotFound
    | 52 -> `PermanentFailure `Gone
    | 53 -> `PermanentFailure `ProxyRequestRefused
    | 59 -> `PermanentFailure `BadRequest
    | 60 -> `ClientCertificateRequired `None
    | 61 -> `ClientCertificateRequired `CertificateNotAuthorised
    | 62 -> `ClientCertificateRequired `CertificateNotValid
    | _ -> raise @@ Invalid_argument "GStatus.of_int"
end

module GHeader = struct
  type t = { status : GStatus.t; meta : string }

  let parse str =
    let re = Str.regexp "\\([0-9][0-9]\\) \\(.*\\)\r\n" in
    if Str.string_match re str 0 then (
      let status = Str.matched_group 1 str |> int_of_string |> GStatus.of_int
      and meta = Str.matched_group 2 str in
      print_endline @@ Str.matched_group 1 str;
      print_endline meta;
      { status; meta })
    else raise @@ Invalid_argument "GHeader.parse"
end
