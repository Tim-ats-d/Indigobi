module Component = Component
module Common = Common
module Gemini = Gemini

let main () =
  let open Component in
  let module Back = Backend.Make (Input.Default) (Requester.Default) in
  let module Front = Frontend.Make (Cli.Default) (Back) in
  Front.get ~url:"gemini://geminispace.info/search" ~host:"geminispace.info"
