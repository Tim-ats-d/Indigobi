(** {1 Types} *)

type err =
  [ `TemporaryFailure of
    string * [ `None | `ServerUnavailable | `CgiError | `ProxyError | `SlowDown ]
  | `PermanentFailure of
    string * [ `None | `NotFound | `Gone | `ProxyRequestRefused | `BadRequest ]
  | `ClientCertificateRequired of
    string * [ `None | `CertificateNotAuthorised | `CertificateNotValid ] ]

type success =
  [ `Input of string * [ `Sensitive of bool ]
  | `Success
  | `Redirect of string * [ `Temporary | `Permanent ] ]

type t = [ success | err ]

(** {2 API} *)

val from_int : string -> int -> (t, Common.Err.status_code) result
val pp : unit -> err -> string
