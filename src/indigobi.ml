module Component = Component
module Common = Common
module Gemini = Gemini

module Cfg : Common.Config.S = struct
  let prompt = LTerm_style.rgb 156 75 191
  let error = LTerm_style.rgb 177 13 13
end

let main () =
  let open Component in
  let module Back = Backend.Make (Input.Make (Cfg)) (Requester.Default) in
  let module Front = Frontend.Make (Cli.Make (Cfg)) (Back) in
  Front.get ~url:"gemini://geminispace.info/search" ~host:"geminispace.info"
