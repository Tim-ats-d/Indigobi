type t = History of hist | Search of search
and hist = { mutable mode : [ `Del of string | `Display | `Search of string ] }

and search = {
  mutable address : string option;
  mutable raw : bool;
  mutable certificate : string;
  mutable timeout : float;
}

module type S = sig
  val parse :
    unit ->
    ( t,
      [> `CliErrBadTimeoutFormat
      | `CliErrUnknownSubCmd of string
      | `CliErrUsageMsg of string ] )
    result
end

module Default : S
