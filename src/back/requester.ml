module G = Gemini

module type S = sig
  type socket

  val init : Gemini.Request.t -> (socket, Common.Err.socket_error) Lwt_result.t
  val close : socket -> unit Lwt.t
  val fetch_header : socket -> string -> string Lwt.t
  val parse_body : socket -> string Lwt.t
end

module Default : S = struct
  open Lwt.Syntax

  type socket = Tls_lwt.ic * Tls_lwt.oc

  let init req =
    try%lwt
      let config =
        let authenticator ?ip:_ ~host = function
          | [] -> Error `EmptyCertificateChain
          | hd :: _ ->
              if
                X509.Certificate.hostnames hd
                |> X509.Host.Set.elements
                |> List.map (fun (_, h) -> Domain_name.to_string h)
                |> List.mem req.G.Request.host
              then
                let open Ptime in
                let validity = X509.Certificate.validity hd in
                let now = Ptime_clock.now () in
                if
                  now |> is_later ~than:(fst validity)
                  && now |> is_earlier ~than:(snd validity)
                then Ok None
                else Error (`LeafCertificateExpired (hd, Some now))
              else Error (`LeafInvalidName (hd, host))
        in
        match req.G.Request.cert with
        | Some certificates -> Tls.Config.client ~authenticator ~certificates ()
        | None -> Tls.Config.client ~authenticator ()
      in
      let socket =
        Tls_lwt.connect_ext config (req.G.Request.host, req.G.Request.port)
      in
      Lwt_result.ok socket
    with
    | Tls_lwt.Tls_failure e -> Lwt_result.fail @@ `Tls e
    | Invalid_argument _ -> Lwt_result.fail @@ `NoAddress req.G.Request.host

  let close (ic, oc) = Lwt.join [ Lwt_io.close ic; Lwt_io.close oc ]
  let input_char (ic, _) = Lwt_io.read_char ic

  let fetch_header (ic, oc) req =
    let* () = Lwt_io.write oc req in
    let buf = Buffer.create 4 in
    let* chr = input_char (ic, oc) in
    Buffer.add_char buf chr;
    let* chr = input_char (ic, oc) in
    Buffer.add_char buf chr;
    let rec input_in () =
      if String.equal Buffer.(sub buf (length buf - 2) 2) "\r\n" then
        Lwt.return @@ Buffer.contents buf
      else
        let* chr = input_char (ic, oc) in
        Buffer.add_char buf chr;
        input_in ()
    in
    input_in ()

  let parse_body socket =
    let buf = Buffer.create 512 in
    let rec input_in () =
      try%lwt
        let* chr = input_char socket in
        Buffer.add_char buf chr;
        input_in ()
      with End_of_file -> Lwt.return @@ Buffer.contents buf
    in
    input_in ()
end
