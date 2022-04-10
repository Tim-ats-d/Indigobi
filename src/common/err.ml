type front = [ `UnknownSubCmd of string | `NoUrlProvided ]

type back =
  [ `MalformedLink
  | `MalformedServerResponse
  | `NotFound
  | `UnknownHostOrServiceName ]

type t = [ front | back ]

let pp () =
  let fmt = Printf.sprintf in
  function
  | `MalformedLink -> fmt "malformed link"
  | `MalformedServerResponse -> fmt "mal formed server response"
  | `NotFound -> fmt "not found"
  | `NoUrlProvided -> fmt "no url is provided"
  | `UnknownHostOrServiceName -> fmt "unknown host or service name"
  | `UnknownSubCmd sub_cmd -> fmt "unknown sub command %S" sub_cmd
