module GRequest : sig
  type t = private { uri : string; addr : Unix.addr_info }

  val create : addr:Unix.addr_info -> string -> t option
  val to_string : t -> string
end

module GStatus : sig
  type t =
    [ `Input of [ `Sensitive of bool ]
    | `Success
    | `Redirect of [ `Temporary | `Permanent ]
    | `TemporaryFailure of
      string
      * [ `None | `ServerUnavailable | `CgiError | `ProxyError | `SlowDown ]
    | `PermanentFailure of
      string * [ `None | `NotFound | `Gone | `ProxyRequestRefused | `BadRequest ]
    | `ClientCertificateRequired of
      string * [ `None | `CertificateNotAuthorised | `CertificateNotValid ] ]

  val of_int : string -> int -> t
end

module GHeader : sig
  type t = { status : GStatus.t; meta : string }

  val parse : string -> t option
end
