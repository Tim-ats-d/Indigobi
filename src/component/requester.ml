module G = Gemini

module type S = sig
  val init : G.Request.t -> Ssl.socket
  val close : Ssl.socket -> unit
  val fetch_header : Ssl.socket -> string -> string
  val parse_body : Ssl.socket -> string
end

module Default : S = struct
  let init req =
    let ctx = Ssl.create_context TLSv1_2 Client_context in
    Ssl.open_connection_with_context ctx req.G.Request.addr.ai_addr

  let close = Ssl.shutdown_connection

  let fetch_header socket req =
    Ssl.output_string socket req;
    let buf = Buffer.create 4 in
    for _ = 0 to 1 do
      (* Status *)
      Buffer.add_char buf @@ Ssl.input_char socket
    done;
    while Buffer.(sub buf (length buf - 2) 2) <> "\r\n" do
      Buffer.add_char buf @@ Ssl.input_char socket
    done;
    Buffer.contents buf

  let parse_body socket =
    let buf = Buffer.create 512 in
    try
      while true do
        Buffer.add_char buf @@ Ssl.input_char socket
      done;
      assert false
    with Ssl.Read_error Error_zero_return -> Buffer.contents buf
end
