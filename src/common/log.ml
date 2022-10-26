module Lvl = struct
  type t = Debug | Info | Warning | Err | Critical

  let pp () = function
    | Debug -> "DEBUG"
    | Info -> "INFO"
    | Warning -> "WARNING"
    | Err -> "ERROR"
    | Critical -> "CRITICAL"
end

module type HANDLER = sig
  val level : Lvl.t
  val formatter : Lvl.t -> string -> string
  val emit : string -> unit Lwt.t
end

module type S = sig
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

let make ~level handlers : (module S) =
  (module struct
    let propage lvl log =
      Lwt_list.iter_s
        (fun (module H : HANDLER) ->
          if H.level <= lvl && level <= lvl then H.emit @@ H.formatter lvl log
          else Lwt.return_unit)
        handlers

    let debug = propage Lvl.Debug
    let info = propage Lvl.Info
    let warn = propage Lvl.Warning
    let err = propage Lvl.Err
    let crit = propage Lvl.Critical
    let debugf fmt args = debug @@ Printf.sprintf fmt args
    let infof fmt args = info @@ Printf.sprintf fmt args
    let warnf fmt args = warn @@ Printf.sprintf fmt args
    let errf fmt args = err @@ Printf.sprintf fmt args
    let critf fmt args = crit @@ Printf.sprintf fmt args
  end)

let fmt lvl log = Printf.sprintf "%a :: %s\n" Lvl.pp lvl log

module StdoutHandler : HANDLER = struct
  let level = Lvl.Debug
  let formatter = fmt
  let emit = Lwt_io.(write stdout)
end

module UserStdoutHandler : HANDLER = struct
  let level = Lvl.Info
  let formatter = fmt
  let emit = Lwt_io.(write stdout)
end

module LogFileHandler : HANDLER = struct
  let level = Lvl.Debug
  let formatter = fmt

  let emit log =
    Lwt_io.with_file ~mode:Output "logbook.log" (fun outc ->
        Lwt_io.write outc log)
end

include (val make ~level:Debug [ (module UserStdoutHandler) ])
