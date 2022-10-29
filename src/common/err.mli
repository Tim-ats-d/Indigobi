type status_code = [ `GracefulFail | `InvalidStatusCode of int ]
(** {1 Types} *)

type ssl_cert_error =
  [ `DomainNameNotPresent of string
  | `ExpiredCertificate
  | `UntrustedCertificate ]

type socket_error = [ `Tls of Tls.Engine.failure | `NoAddress of string ]
type header = [ status_code | `MalformedHeader | `TooLongHeader ]

type back =
  [ status_code
  | ssl_cert_error
  | `SocketError of socket_error
  | `InvalidClientCertificate of string
  | `FileNotFound of string
  | `MalformedLink
  | `MalformedServerResponse
  | `NotFound
  | `Timeout
  | `UnknownHostOrServiceName ]

type front = [ `BadTimeoutFormat ]
type err = [ front | back ]
type 'a or_error = ('a, err) result

(** {2 API} *)

val pp : unit -> err -> string
