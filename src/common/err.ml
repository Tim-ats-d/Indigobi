type status_code = [ `GracefulFail | `InvalidStatusCode of int ]

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

let pp () = function
  | `GracefulFail -> "graceful fail: server returns 99 status code"
  | `InvalidStatusCode code ->
      Printf.sprintf "server returns invalid status code \"%i\"" code
  | `DomainNameNotPresent domain ->
      Printf.sprintf "domain %s not present in certificate" domain
  | `ExpiredCertificate -> "expired certificate"
  | `UntrustedCertificate -> "untrusted certificate"
  | `MalformedLink -> "malformed link"
  | `MalformedServerResponse -> "mal formed server response"
  | `NotFound -> "not found"
  | `Timeout -> "timeout"
  | `UnknownHostOrServiceName -> "unknown host or service name"
  | `BadTimeoutFormat -> "bad format for timeout: a float is expected"
  | `InvalidClientCertificate reason ->
      Printf.sprintf "invalid client certificate: %S" reason
  | `FileNotFound file -> Printf.sprintf "file \"%s\" not found" file
  | `SocketError (`Tls err) -> raise (Tls_lwt.Tls_failure err)
  | `SocketError (`NoAddress address) ->
      Printf.sprintf "no address for %s" address
