type t =
  [ `MalformedLink
  | `MalformedServerResponse
  | `NotFound
  | `NoUrlProvided
  | `UnknownHostOrServiceName ]

val show : t -> string
