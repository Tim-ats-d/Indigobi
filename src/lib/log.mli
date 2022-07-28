val debug : string -> unit Lwt.t
val info : string -> unit Lwt.t
val warn : string -> unit Lwt.t
val err : string -> unit Lwt.t
val crit : string -> unit Lwt.t
val debugf : (string -> string, unit, string) format -> string -> unit Lwt.t
val infof : (string -> string, unit, string) format -> string -> unit Lwt.t
val warnf : (string -> string, unit, string) format -> string -> unit Lwt.t
val errf : (string -> string, unit, string) format -> string -> unit Lwt.t
val critf : (string -> string, unit, string) format -> string -> unit Lwt.t
