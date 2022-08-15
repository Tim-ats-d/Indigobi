type status_code = [ `GracefulFail | `InvalidStatusCode of int ]

type ssl_cert_error =
  [ `MismatchedDomainNames of string * string
  | `ExpiredCertificate
  | `UntrustedCertificate ]

type header = [ status_code | `MalformedHeader | `TooLongHeader ]

type back =
  [ status_code
  | ssl_cert_error
  | `Tls of Tls.Engine.failure
  | `InvalidClientCertificate of string
  | `FileNotFound of string
  | `MalformedLink
  | `MalformedServerResponse
  | `NotFound
  | `Timeout
  | `UnknownHostOrServiceName ]

type front =
  [ `CliErrBadTimeoutFormat | `CliErrUnknownSubCmd of string | `NoUrlProvided ]

type t = [ front | back ]

val pp : unit -> t -> string
