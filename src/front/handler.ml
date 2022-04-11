module type S = sig
  val handle_text : ?typ:string -> string -> unit Lwt.t
  val handle_gemini : Gemini.Text.t -> unit Lwt.t
  val handle_other : string -> string -> unit Lwt.t

  val handle_err :
    [< `GeminiErr of Gemini.Status.err | `CommonErr of Common.Err.t ] ->
    unit Lwt.t
end

module Make (Printer : Printer.S) : S = struct
  let handle_text ?typ = Lwt_io.printl [@@warning "-27"]

  open Lwt.Syntax

  let handle_gemini lines =
    let print_line line =
      let* term = Lazy.force LTerm.stdout in
      let* () =
        try LTerm.fprintls term @@ Printer.stylize_gemini line
        with Zed_string.Invalid (_, text) -> LTerm.printl text
      in
      LTerm.flush term
    in
    Lwt_list.iter_s print_line lines

  let handle_other _ _ = Lwt.return @@ failwith "todo: non-text format"

  let handle_err err =
    let msg =
      match err with
      | `GeminiErr g -> Printf.sprintf "Gemini error: %a" Gemini.Status.pp g
      | `CommonErr err -> Printf.sprintf "Error: %a" Common.Err.pp err
    in
    let* term = Lazy.force LTerm.stderr in
    let* () = LTerm.fprintls term @@ Printer.stylize_error msg in
    LTerm.flush term
end
