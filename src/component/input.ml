module type S = sig
  val input : string -> string
  val sensitive : string -> string
end

let fmt_prompt = Printf.sprintf "%s: "

module Default : S = struct
  let input prompt =
    print_string @@ fmt_prompt prompt;
    flush stdout;
    Common.Urllib.encode @@ input_line stdin

  class hidden_read ~prompt term =
    object (self)
      inherit LTerm_read_line.read_password () as super
      inherit [Zed_string.t] LTerm_read_line.term term

      initializer
      fmt_prompt prompt |> LTerm_text.of_utf8 |> Lwt_react.S.const
      |> self#set_prompt

      method! send_action =
        function
        | LTerm_read_line.Break -> () | action -> super#send_action action
    end

  let sensitive prompt =
    let launch () =
      let ( >>= ) = Lwt.( >>= ) in
      LTerm_inputrc.load () >>= fun () ->
      Lazy.force LTerm.stdout >>= fun term ->
      (new hidden_read ~prompt term)#run >>= fun input ->
      Lwt.return @@ Zed_string.to_utf8 input
    in
    Lwt_main.run (launch ())
end
