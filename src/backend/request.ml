type bypass = {
  host : bool;
  expiration : bool;
  empty : bool;
  fingerprint : bool;
}

type t = { uri : Uri.t; cert : Tls.Config.own_cert option; bypass : bypass }

let default_bypass =
  { host = false; expiration = false; empty = false; fingerprint = false }

let create ~bypass ~cert uri =
  if Bytes.(length @@ of_string @@ Uri.to_string uri) > 1024 then None
  else Some { uri; cert; bypass }

let attach_input t input = { t with uri = Uri.with_query t.uri [ (input, []) ] }
let to_string { uri; _ } = Uri.to_string uri ^ "\r\n"
