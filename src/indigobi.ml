module Component = Component
module Common = Common
module Gemini = Gemini
open Import

(* let url = "gemini://geminispace.info/"
   and host = "geminispace.info" *)

(* let url = "gemini://geminispace.info/search"
   and host = "geminispace.info" *)

let url = "gemini://iich.space/"
and host = "iich.space"

let main () =
  let module Back = Backend.Make (Input.Make (Default.Cfg)) (Requester.Default)
  in
  let module Front = Frontend.Make (Cli.Make (Default.Cfg)) (Back) in
  Lwt_main.run @@ Front.search_and_display ~url ~host
