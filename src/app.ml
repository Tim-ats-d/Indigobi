module Back = Back
module Front = Front
module Common = Common
module Gemini = Gemini
module Lib = Lib

module type S = functor
  (ExtHandler : functor (P : Front.Prompt.S) -> Front.ExtHandler.S)
  -> sig
  val main : unit -> unit
end

module Make (ExtHandler : functor (P : Front.Prompt.S) -> Front.ExtHandler.S) =
struct
  let main () =
    let open Import in
    let module Printer = Printer.Make (Config.Theme.Default) in
    let module Prompt = Prompt.Make (Printer) in
    let module Back = Backend.Make (Prompt) (Requester.Default) in
    let module Front =
      Frontend.Make (Back) (Handler.Make (Printer) (ExtHandler.Make (Prompt)))
        (Cli.Default)
    in
    Lwt_main.run @@ Front.launch ()
end
