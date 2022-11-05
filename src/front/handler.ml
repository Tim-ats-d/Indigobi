module type S = sig
  val handle_text : ?typ:string -> string -> unit Lwt.t
  val handle_gemini : 'a Context.t -> Gemini.Gemtext.t -> unit Lwt.t
  val handle_other : string -> mime:string -> unit Lwt.t

  val handle_err :
    [< `GeminiErr of Gemini.Status.err | `CommonErr of Common.Err.t ] ->
    unit Lwt.t
end

module Make (Printer : Printer.S) (ExternalHandler : ExtHandler.S) : S = struct
  open Lwt.Syntax

  let with_term ic f =
    let* term = Lazy.force ic in
    let* () = f term in
    LTerm.flush term

  let handle_text ?typ:_ = LTerm.printl

  let handle_gemini ctx lines =
    let print_line line =
      with_term LTerm.stdout (fun term ->
          try%lwt
            let* stylized_line = Printer.stylize_gemini ~ctx line in
            LTerm.fprintls term stylized_line
          with Zed_string.Invalid (_, text) -> LTerm.printl text)
    in
    Lwt_list.iter_s print_line lines

  let handle_other = ExternalHandler.handle

  let handle_err err =
    let msg =
      match err with
      | `GeminiErr g -> Printf.sprintf "Gemini error: %a" Gemini.Status.pp g
      | `CommonErr err -> Printf.sprintf "Error: %a" Common.Err.pp err
    in
    with_term LTerm.stderr (fun term ->
        LTerm.fprintls term @@ Printer.stylize_error msg)
end
