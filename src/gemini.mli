module GRequest : sig
  type t = { uri : string; addr : Unix.addr_info }

  val create : addr:Unix.addr_info -> string -> t option
  val to_string : t -> string
end

module GStatus : sig
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

  val of_int : string -> int -> t
  val show : err -> string
end

module GHeader : sig
  type t = { status : GStatus.t; meta : string }

  val parse : string -> t option
end
