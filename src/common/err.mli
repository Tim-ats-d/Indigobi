type t =
  [ `MalformedLink
  | `MalformedServerResponse
  | `NotFound
  | `UnknownHostOrServiceName
  | `TooManyAddressSpecified ]

val show : t -> string
