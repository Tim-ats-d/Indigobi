module Component = Component
module Common = Common
module Gemini = Gemini
open Import

let main () =
   let module Back = Backend.Make (Input.Make (Default.Cfg)) (Requester.Default)
   in
   let module Front = Frontend.Make (Cli.Make (Default.Cfg)) (Back) in
   (* Front.get ~url:"gemini://geminispace.info/" ~host:"geminispace.info"; *)
   Front.get ~url:"gemini://geminispace.info/search" ~host:"geminispace.info";
   Front.get ~url:"gemini://iich.space/" ~host:"iich.space"
