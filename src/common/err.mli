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

val pp : unit -> t -> string
