type bypass = {
  host : bool;
  expiration : bool;
  empty : bool;
  fingerprint : bool;
}

type t = {
  base_url : string;
  uri : string;
  host : string;
  port : int;
  cert : Tls.Config.own_cert option;
  bypass : bypass;
}

let default_bypass =
  { host = false; expiration = false; empty = false; fingerprint = false }

let create ~bypass ~host ~port ~cert url =
  if Bytes.(length @@ of_string url) > 1024 then None
  else
    let uri = url ^ "\r\n" in
    Some { base_url = url; uri; host; port; cert; bypass }

let attach_input t input =
  { t with uri = Printf.sprintf "%s?%s\r\n" t.base_url input }

let to_string { uri; _ } = uri
