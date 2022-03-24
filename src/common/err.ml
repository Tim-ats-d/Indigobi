type t =
  [ `MalformedLink
  | `MalformedServerResponse
  | `NotFound
  | `UnknownHostOrServiceName
  | `TooManyAddressSpecified ]

let show = function
  | `MalformedLink -> "malformed link"
  | `MalformedServerResponse -> "mal formed server response"
  | `NotFound -> "not found"
  | `UnknownHostOrServiceName -> "unknown host or service name"
  | `TooManyAddressSpecified -> "too many address specified"
