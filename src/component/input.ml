open Common

module type S = sig
  val input : string -> string
  val sensitive : string -> string
end

module Make (Cfg : Config.S) = struct
  let make_prompt meta =
    let msg = meta ^ " " in
    LTerm_text.eval [ B_fg Cfg.prompt; S msg; E_fg ]

  let input meta =
    make_prompt meta |> LTerm.prints |> Lwt_main.run;
    try Urllib.encode @@ input_line stdin with End_of_file -> exit 1

  class hidden_read ~prompt term =
    object (self)
      inherit LTerm_read_line.read_password () as super
      inherit [Zed_string.t] LTerm_read_line.term term
      initializer prompt |> Lwt_react.S.const |> self#set_prompt

      method! send_action =
        function
        | LTerm_read_line.Break -> () | action -> super#send_action action
    end

  let sensitive meta =
    let launch () =
      let ( >>= ) = Lwt.( >>= ) in
      LTerm_inputrc.load () >>= fun () ->
      Lazy.force LTerm.stdout >>= fun term ->
      LTerm.flush term >>= fun () ->
      let prompt = make_prompt meta in
      (new hidden_read ~prompt term)#run >>= fun input ->
      Zed_string.to_utf8 input |> Urllib.encode |> Lwt.return
    in
    Lwt_main.run @@ launch ()
end
