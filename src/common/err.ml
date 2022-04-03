type front = [ `UnknownSubCmd of string | `NoUrlProvided ]

type back =
  [ `MalformedLink
  | `MalformedServerResponse
  | `NotFound
  | `UnknownHostOrServiceName ]

type t = [ front | back ]

let show = function
  | `MalformedLink -> "malformed link"
  | `MalformedServerResponse -> "mal formed server response"
  | `NotFound -> "not found"
  | `NoUrlProvided -> "no url is provided"
  | `UnknownHostOrServiceName -> "unknown host or service name"
  | `UnknownSubCmd sub_cmd ->
      Printf.sprintf "unknown sub command \"%s\"" sub_cmd
