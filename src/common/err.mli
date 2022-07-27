type cli =
  [ `CliErrBadTimeoutFormat
  | `CliErrUnknownSubCmd of string
  | `CliErrUsageMsg of string ]

type status_code = [ `GracefulFail | `InvalidStatusCode of int ]
type header = [ status_code | `MalformedHeader | `TooLongHeader ]

type back =
  [ status_code
  | `MalformedLink
  | `MalformedServerResponse
  | `NotFound
  | `UnknownHostOrServiceName ]

type front =
  [ `CliErrBadTimeoutFormat | `CliErrUnknownSubCmd of string | `NoUrlProvided ]

type t = [ front | back ]

val pp : unit -> t -> string
