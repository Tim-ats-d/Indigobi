type t =
  [ `MalformedLink
  | `MalformedServerResponse
  | `NotFound
  | `NoUrlProvided
  | `UnknownHostOrServiceName ]

let show = function
  | `MalformedLink -> "malformed link"
  | `MalformedServerResponse -> "mal formed server response"
  | `NotFound -> "not found"
  | `NoUrlProvided -> "no url is provided"
  | `UnknownHostOrServiceName -> "unknown host or service name"
