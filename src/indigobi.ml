module Back = Back
module Front = Front
module Common = Common
module Gemini = Gemini
module Lib = Lib

let main () =
  let open Import in
  let module Front = Frontend.Make (Cli.Default) in
  Printexc.record_backtrace true;
  Printexc.print_backtrace stdout;
  Lwt_main.run @@ Front.launch ()
