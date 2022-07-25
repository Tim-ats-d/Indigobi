module Lvl : sig
  type t = Debug | Info | Warning | Err | Critical

  val pp : unit -> t -> string
end

module type HANDLER = sig
  val level : Lvl.t
  val formatter : Lvl.t -> string -> string
  val emit : string -> unit Lwt.t
end

module type S = sig
  val level : Lvl.t
  val handlers : (module HANDLER) list
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
end

val make : level:Lvl.t -> (module HANDLER) list -> (module S)
