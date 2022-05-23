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

val of_int : string -> int -> t
val pp : unit -> err -> string
