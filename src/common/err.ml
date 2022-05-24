type front = [ `UnknownSubCmd of string | `NoUrlProvided ]
type status_code = [ `GracefulFail | `InvalidStatusCode of int ]
type header = [ status_code | `MalformedHeader | `TooLongHeader ]

type back =
  [ status_code
  | `MalformedLink
  | `MalformedServerResponse
  | `NotFound
  | `UnknownHostOrServiceName ]

type t = [ front | back ]

let pp () = function
  | `GracefulFail -> "graceful fail: server returns 99 status code"
  | `InvalidStatusCode code ->
      Printf.sprintf "server returns invalid status code \"%i\"" code
  | `MalformedLink -> "malformed link"
  | `MalformedServerResponse -> "mal formed server response"
  | `NotFound -> "not found"
  | `NoUrlProvided -> "no url is provided"
  | `UnknownHostOrServiceName -> "unknown host or service name"
  | `UnknownSubCmd sub_cmd -> Printf.sprintf "unknown sub command %S" sub_cmd
