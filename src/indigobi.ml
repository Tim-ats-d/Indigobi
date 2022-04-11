module Back = Back
module Front = Front
module Common = Common
module Gemini = Gemini

let main () =
  let open Import in
  let module Printer = Printer.Make (Config.Theme.Default) in
  let module Back = Backend.Make (Prompt.Make (Printer)) (Requester.Default) in
  let module Front = Frontend.Make (Back) (Handler.Make (Printer)) (Cli.Default)
  in
  Lwt_main.run @@ Front.launch ()
