module type S = sig
  val prompt : string -> string Lwt.t
  val prompt_sensitive : string -> string Lwt.t
  val prompt_bool : string -> bool Lwt.t
end

module Make (Printer : Printer.S) = struct
  open Lwt.Syntax

  let prompt meta =
    let* term = Lazy.force LTerm.stdout in
    let* () = LTerm.fprints term @@ Printer.stylize_prompt meta in
    let* () = LTerm.flush term in
    let* user_input = Lwt_io.(read_line stdin) in
    let* () = LTerm.clear_line_prev term in
    Lwt.return user_input

  class hidden_read ~prompt term =
    object (self)
      inherit LTerm_read_line.read_password () as super
      inherit [Zed_string.t] LTerm_read_line.term term
      initializer prompt |> Lwt_react.S.const |> self#set_prompt

      method! send_action =
        function
        | LTerm_read_line.Break -> exit 130 | action -> super#send_action action
    end

  let prompt_sensitive meta =
    let* term = Lazy.force LTerm.stdout in
    let* input =
      (new hidden_read ~prompt:(Printer.stylize_prompt meta) term)#run
    in
    let* () = LTerm.flush term in
    let* () = LTerm.clear_line_prev term in
    Lwt.return @@ Zed_string.to_utf8 input

  let prompt_bool meta =
    let rec run term =
      let* () = LTerm.fprint term @@ Printf.sprintf "%s [Y/n] " meta in
      let* event = LTerm.read_event term in
      match event with
      | LTerm_event.Key LTerm_key.{ code = Char ch; control = true; _ }
        when ch = Uchar.of_char 'c' ->
          exit 1
      | LTerm_event.Key LTerm_key.{ code = Char ch; _ } -> (
          let resp = Zed_utf8.singleton ch in
          let* () = LTerm.fprintf term "%s\n" resp in
          match resp with
          | "Y" | "y" -> Lwt.return_true
          | "N" | "n" -> Lwt.return_false
          | _ -> run term)
      | _ -> run term
    in
    let* term = Lazy.force LTerm.stdout in
    let* mode = LTerm.enter_raw_mode term in
    Lwt.finalize (fun () -> run term) (fun () -> LTerm.leave_raw_mode term mode)
end
