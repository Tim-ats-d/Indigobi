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

type front =
  [ `CliErrBadTimeoutFormat | `CliErrUnknownSubCmd of string | `NoUrlProvided ]

type t = [ front | back ]

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
  | `NoUrlProvided -> "no url is provided"
  | `UnknownHostOrServiceName -> "unknown host or service name"
  | `CliErrBadTimeoutFormat -> "bad format for timeout"
  | `CliErrUnknownSubCmd sub_cmd ->
      Printf.sprintf "unknown sub command %S" sub_cmd
  | `InvalidClientCertificate reason ->
      Printf.sprintf "invalid client certificate: %S" reason
  | `FileNotFound file -> Printf.sprintf "file \"%s\" not found" file
  | `SocketError error -> (
      match error with
      | `Tls tls_error -> raise @@ Tls_lwt.Tls_failure tls_error
      | `NoAddress address -> Printf.sprintf "no address for %s" address)
