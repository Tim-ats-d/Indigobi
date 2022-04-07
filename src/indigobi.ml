module Component = Component
module Common = Common
module Gemini = Gemini

let main () =
  let open Import in
  let module Cfg = Config.Make (Config.Theme.Default) in
  let module Back = Backend.Make (Prompt.Make (Cfg)) (Requester.Default) in
  let module Front = Frontend.Make (Back) (Printer.Make (Cfg)) (Cli.Default) in
  Lwt_main.run @@ Front.launch ()
