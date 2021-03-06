open Common

module type S = sig
  val prompt : string -> string Lwt.t
  val prompt_sensitive : string -> string Lwt.t
end

module Make (Printer : Printer.S) = struct
  open Lwt.Syntax

  let prompt meta =
    let* term = Lazy.force LTerm.stdout in
    let* () = LTerm.fprints term @@ Printer.stylize_prompt meta in
    let* () = LTerm.flush term in
    let* user_input = Lwt_io.(read_line stdin) in
    let* () = LTerm.clear_line_prev term in
    Lwt.return @@ Urllib.encode @@ user_input

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
    Lwt.return @@ Urllib.encode @@ Zed_string.to_utf8 input
end
