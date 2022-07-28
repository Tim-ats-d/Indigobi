type status_code = [ `GracefulFail | `InvalidStatusCode of int ]

type ssl_cert_error =
  [ `MismatchedDomainNames of string * string
  | `CertificateExpired
  | `UntrustedCertificate ]

type header = [ status_code | `MalformedHeader | `TooLongHeader ]

type back =
  [ status_code
  | ssl_cert_error
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
  | `MismatchedDomainNames (req_cn, cert_cn) ->
      Printf.sprintf "mismatched domain names: %S (request), %S (certificate)"
        req_cn cert_cn
  | `CertificateExpired -> "certificate expired"
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
