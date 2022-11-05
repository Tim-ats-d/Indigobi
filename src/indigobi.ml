module Back = Back
module Front = Front
module Common = Common
module Gemini = Gemini
module Lib = Lib

let main () =
  let module Main = App.Make (Front.ExtHandler.Make) in
  Printexc.record_backtrace true;
  Printexc.print_backtrace stdout;
  Main.main ()
