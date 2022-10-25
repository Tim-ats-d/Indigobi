module Attr = Notty.A
module Img = Notty.I
module Term = Notty_lwt.Term
open Lwt.Syntax

let handle_event term = function
  | `Key (`ASCII 'q', []) | `Key (`ASCII 'C', [ `Ctrl ]) -> Term.release term
  | `Resize _ -> Term.image term (Img.string Attr.empty "Resized")
  | _ -> Lwt.return_unit

let mainloop () =
  let term = Term.create () in
  let* () = Term.image term (Img.string Attr.empty "foo") in
  Lwt_stream.iter_s (handle_event term) (Term.events term)

let main () = Lwt_main.run @@ mainloop ()
