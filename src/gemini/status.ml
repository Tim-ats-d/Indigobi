type err =
  [ `TemporaryFailure of
    string * [ `None | `ServerUnavailable | `CgiError | `ProxyError | `SlowDown ]
  | `PermanentFailure of
    string * [ `None | `NotFound | `Gone | `ProxyRequestRefused | `BadRequest ]
  | `ClientCertificateRequired of
    string * [ `None | `CertificateNotAuthorised | `CertificateNotValid ] ]

type t =
  [ `Input of string * [ `Sensitive of bool ]
  | `Success
  | `Redirect of string * [ `Temporary | `Permanent ]
  | err ]

let from_int meta = function
  | 10 -> Ok (`Input (meta, `Sensitive false))
  | 11 -> Ok (`Input (meta, `Sensitive true))
  | 20 -> Ok `Success
  | 30 -> Ok (`Redirect (meta, `Temporary))
  | 31 -> Ok (`Redirect (meta, `Permanent))
  | 40 -> Ok (`TemporaryFailure (meta, `None))
  | 41 -> Ok (`TemporaryFailure (meta, `ServerUnavailable))
  | 42 -> Ok (`TemporaryFailure (meta, `CgiError))
  | 43 -> Ok (`TemporaryFailure (meta, `ProxyError))
  | 44 -> Ok (`TemporaryFailure (meta, `SlowDown))
  | 50 -> Ok (`PermanentFailure (meta, `None))
  | 51 -> Ok (`PermanentFailure (meta, `NotFound))
  | 52 -> Ok (`PermanentFailure (meta, `Gone))
  | 53 -> Ok (`PermanentFailure (meta, `ProxyRequestRefused))
  | 59 -> Ok (`PermanentFailure (meta, `BadRequest))
  | 60 -> Ok (`ClientCertificateRequired (meta, `None))
  | 61 -> Ok (`ClientCertificateRequired (meta, `CertificateNotAuthorised))
  | 62 -> Ok (`ClientCertificateRequired (meta, `CertificateNotValid))
  | 99 -> Error `GracefulFail
  | code -> Error (`InvalidStatusCode code)

let pp () =
  let fmt = Printf.sprintf in
  function
  | `TemporaryFailure (m, err) ->
      fmt "temporary failure: %s%s"
        (match err with
        | `None -> ""
        | `ServerUnavailable -> "server unavailable: "
        | `CgiError -> "CGI error: "
        | `ProxyError -> "proxy error: "
        | `SlowDown -> "slow down: ")
        m
  | `PermanentFailure (m, err) ->
      fmt "permanent failure: %s%s"
        (match err with
        | `None -> ""
        | `NotFound -> "not found: "
        | `Gone -> "gone: "
        | `ProxyRequestRefused -> "proxy request refused: "
        | `BadRequest -> "bad request: ")
        m
  | `ClientCertificateRequired (m, err) ->
      fmt "client certificate required: %s%s"
        (match err with
        | `None -> ""
        | `BadRequest -> "bad request: "
        | `CertificateNotAuthorised -> "cerficate not authorised: "
        | `CertificateNotValid -> "cerficate not valid: ")
        m
