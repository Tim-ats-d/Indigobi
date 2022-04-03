type front = [ `UnknownSubCmd of string | `NoUrlProvided ]

type back =
  [ `MalformedLink
  | `MalformedServerResponse
  | `NotFound
  | `UnknownHostOrServiceName ]

type t = [ front | back ]

val show : t -> string
