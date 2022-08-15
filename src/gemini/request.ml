type t = {
  base_url : string;
  uri : string;
  host : string;
  port : int;
  cert : string;
}

let create ~host ~port ~cert url =
  if Bytes.(length @@ of_string url) > 1024 then None
  else
    let uri = url ^ "\r\n" in
    Some { base_url = url; uri; host; port; cert }

let attach_input t input =
  { t with uri = Printf.sprintf "%s?%s\r\n" t.base_url input }

let to_string { uri; _ } = uri
