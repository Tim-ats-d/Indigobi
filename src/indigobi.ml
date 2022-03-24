module Component = Component
module Common = Common
module Gemini = Gemini
open Import

let main () =
  let module Back = Backend.Make (Input.Make (Default.Cfg)) (Requester.Default)
  in
  let module Front =
    Frontend.Make (Cli.Make (Default.Cfg)) (Back) (Args.Default)
  in
  Lwt_main.run @@ Front.search_and_display ~default_url:"gemini://iich.space/"
