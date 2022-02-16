module Err = struct
  type err = [ `UnknownHostOrServiceName | `MalformedUrl | `NotFound ]
end

module Option = struct
  include Option

  let ( let* ) = Result.bind
end

module GRequest : sig
  type t = private { uri : string; addr : Unix.addr_info }

  val create : addr:Unix.addr_info -> string -> t option
  val to_string : t -> string
end = struct
  type t = { uri : string; addr : Unix.addr_info }

  let create ~addr url =
    if Bytes.(length @@ of_string url) > 1024 then None
    else Some { uri = url ^ "\r\n"; addr }

  let to_string { uri; _ } = uri
end

let get_header req =
  let module B = Bytes in
  try
    let ctx = Ssl.create_context TLSv1_2 Client_context in
    let socket =
      Ssl.open_connection_with_context ctx req.GRequest.addr.ai_addr
    in
    Ssl.accept socket;
    Ssl.output_string socket @@ GRequest.to_string req;
    let header = B.create 1050 in
    let _ = Ssl.read socket header 0 @@ B.length header in
    let body = Bytes.create 10000 in
    let _ = Ssl.read socket body 0 @@ B.length body in
    Ssl.shutdown_connection socket;
    Some B.(to_string header, B.to_string body)
  with Unix.Unix_error _ -> None

type 'a state = Found of 'a | NotFound | ReqErr

let get ~url ~host =
  Ssl.init ();
  let addr_info = Unix.getaddrinfo host "1965" [] in
  match addr_info with
  | [] -> Error `UnknownHostOrServiceName
  | address -> (
      let response =
        List.fold_left
          (fun acc addr ->
            match acc with
            | NotFound -> (
                match GRequest.create url ~addr with
                | None -> ReqErr
                | Some r -> (
                    match get_header r with
                    | None -> NotFound
                    | Some s -> Found s))
            | other -> other)
          NotFound address
      in
      match response with
      | Found f -> Ok f
      | NotFound -> Error `Not_found
      | ReqErr -> Error `MalformedUrl)

let () =
  match
    get ~url:"gemini://gemini.circumlunar.space/news/"
      ~host:"gemini.circumlunar.space"
  with
  | Ok (header, body) -> Printf.printf "%s\n---\n%s" header body
  | Error _ -> print_endline "error"
