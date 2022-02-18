type t = { status : Status.t; meta : string }

val parse : string -> t option
