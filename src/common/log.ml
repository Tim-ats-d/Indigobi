open Logger

let fmt lvl log = Printf.sprintf "%a :: %s\n" Lvl.pp lvl log

module OutChanHandler : HANDLER = struct
  let level = Lvl.Debug
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

include (val Logger.make ~level:Debug [])
