module type S = sig
  val handle_text : ?typ:string -> string -> unit Lwt.t
  val handle_gemini : Gemini.Text.t -> unit Lwt.t
  val handle_other : string -> string -> unit Lwt.t

  val handle_err :
    [< `GeminiErr of Gemini.Status.err | `CommonErr of Common.Err.t ] ->
    unit Lwt.t
end

open Common

module Make
    (Cfg : Config.S
             with type t := Gemini.Text.line
              and type color = LTerm_style.color
              and type markup = LTerm_text.markup) : S = struct
  let handle_text ?typ = Lwt_io.printl [@@warning "-27"]

  open Lwt.Syntax

  let handle_gemini lines =
    let print_line line =
      let* term = Lazy.force LTerm.stdout in
      let* () =
        try Cfg.to_markup line |> LTerm_text.eval |> LTerm.fprintls term
        with Zed_string.Invalid (_, text) -> LTerm.printl text
      in
      LTerm.flush term
    in
    Lwt_list.iter_s print_line lines

  let handle_other _ _ = Lwt.return @@ failwith "todo: non-text format"

  let handle_err err =
    let msg =
      match err with
      | `GeminiErr g -> "Gemini error: " ^ Gemini.Status.show g
      | `CommonErr err -> "Error: " ^ Common.Err.show err
    in
    let color_msg = LTerm_text.eval [ B_fg Cfg.error_clr; S msg; E_fg ] in
    let* term = Lazy.force LTerm.stderr in
    let* () = LTerm.fprintls term color_msg in
    LTerm.flush term
end
