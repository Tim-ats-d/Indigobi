module GRequest = struct
  type t = { uri : string; addr : Unix.addr_info }

  let create ~addr url =
    if Bytes.(length @@ of_string url) > 1024 then None
    else Some { uri = url ^ "\r\n"; addr }

  let to_string { uri; _ } = uri
end

module GStatus = struct
  type t = string
end

module GHeader = struct
  type t = { status : GStatus.t; mime : string; meta : string }

  let parse s =
    let ria () = raise @@ Invalid_argument "GHeader.parse" in
    match String.split_on_char ' ' s with
    | [ status; carret ] -> (
        match String.split_on_char '\n' carret with
        | [ mime; meta ] -> { status; mime; meta }
        | _ -> ria ())
    | _ -> ria ()
end
