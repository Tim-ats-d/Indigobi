module type S = sig
  type socket

  val init : Request.t -> (socket, Common.Err.socket_error) Lwt_result.t
  val close : socket -> unit Lwt.t
  val fetch_header : socket -> string -> string Lwt.t
  val parse_body : socket -> string Lwt.t
end

module Default : S = struct
  open Lwt.Syntax

  type socket = Tls_lwt.ic * Tls_lwt.oc

  let init req =
    let* tofu_entry = Tofu.get_by_host req.Request.host in
    let new_tofu_entry =
      (* This variable should not be accessed outside `authenticator` and `Tofu.save_entry` *)
      ref { Tofu.host = ""; fingerprint = ""; expiration_date = 0.0 }
    in
    try%lwt
      let config =
        let authenticator ?ip:_ ~host certs =
          let cert = List.hd certs in
          let entry =
            Option.value
              ~default:
                {
                  Tofu.host = req.host;
                  fingerprint =
                    X509.Certificate.fingerprint `SHA256 cert
                    |> Cstruct.to_string;
                  expiration_date =
                    X509.Certificate.validity cert |> snd |> Ptime.to_float_s;
                }
              tofu_entry
          in
          let validation =
            X509.Validation.trust_cert_fingerprint ~host
              ~time:(fun () -> Some (Ptime_clock.now ()))
              ~hash:`SHA256
              ~fingerprint:
                (Cstruct.of_string
                @@
                match tofu_entry with
                | None -> entry.fingerprint
                | Some e -> e.fingerprint)
              certs
          in

          match validation with
          | Ok _ ->
              new_tofu_entry := Option.value ~default:entry tofu_entry;
              validation
          | Error e -> (
              match e with
              | `LeafInvalidName _ when req.bypass.host -> Ok None
              | `LeafCertificateExpired _ when req.bypass.expiration -> Ok None
              | `EmptyCertificateChain when req.bypass.empty -> Ok None
              | `InvalidFingerprint _ when req.bypass.fingerprint -> Ok None
              | _ -> validation)
        in
        match req.cert with
        | None -> Tls.Config.client ~authenticator ()
        | Some certificates -> Tls.Config.client ~authenticator ~certificates ()
      in
      let* socket = Tls_lwt.connect_ext config (req.host, req.port) in
      let* () = Tofu.save_entry !new_tofu_entry in
      Lwt_result.return socket
    with
    | Tls_lwt.Tls_failure e -> Lwt_result.fail @@ `Tls e
    | Invalid_argument _ -> Lwt_result.fail @@ `NoAddress req.host

  let close (ic, oc) = Lwt.join [ Lwt_io.close ic; Lwt_io.close oc ]
  let input_char (ic, _) = Lwt_io.read_char ic

  let fetch_header (ic, oc) req =
    let* () = Lwt_io.write oc req in
    Lwt_io.read_line ic

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
