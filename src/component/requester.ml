module G = Gemini

module type S = sig
  val init : G.Request.t -> Ssl.context
  val fetch_header : Ssl.context -> string -> string
  val parse_body : Ssl.context -> string
end

module Default : S = struct
  let init req =
    let ctx = Ssl.create_context () in
    (if req.G.Request.cert <> "" then
     let cert_re =
       Str.regexp
         "\\(-----BEGIN CERTIFICATE-----.+-----END CERTIFICATE-----\\) \
          \\(-----BEGIN PRIVATE KEY-----.+-----END PRIVATE KEY-----\\)"
     in
     Ssl.use_certificate_from_string ctx
       (Str.replace_first cert_re "\\1" req.G.Request.cert)
       (Str.replace_first cert_re "\\2" req.G.Request.cert));
    Ssl.open_connection_with_context ctx req.G.Request.host req.G.Request.port;
    ctx

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
