type front = [ `NoUrlProvided ]

type back =
  [ `MalformedLink
  | `MalformedServerResponse
  | `NotFound
  | `UnknownHostOrServiceName ]

type t = [ front | back ]

val show : t -> string
