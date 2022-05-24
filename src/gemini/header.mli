type t = { status : Status.t; meta : string }

val parse : string -> (t, Common.Err.header) result
