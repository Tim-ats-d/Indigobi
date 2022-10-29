let main () =
  let module Prompt = Frontend.Prompt.Make (Frontend.Printer.Default) in
  let module Back = Back.Make (Prompt) (Backend.Requester.Default) in
  let module Front =
    Front.Make (Back) (Frontend.Cli.Default) (Frontend.Printer.Default)
  in
  Printexc.record_backtrace true;
  Printexc.print_backtrace stdout;
  Front.main ()
