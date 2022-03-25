module Component = Component
module Common = Common
module Gemini = Gemini
open Import

let main () =
  let module Back = Backend.Make (Input.Make (Default.Cfg)) (Requester.Default)
  in
  let module Front =
    Frontend.Make (Back) (Pprint.Make (Default.Cfg)) (Args.Default)
  in
  Lwt_main.run @@ Front.launch ()
