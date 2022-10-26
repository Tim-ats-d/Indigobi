type t = {
  scheme : string;
  domain : string;
  port : int;
  path : string;
  query : string;
}

let encode url =
  let convert_char buf chr =
    Buffer.add_string buf
      (match chr with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> Printf.sprintf "%c" chr
      | _ -> Printf.sprintf "%%%02X" @@ Char.code chr);
    buf
  in
  String.fold_left convert_char (Buffer.create 101) url |> Buffer.contents

module Re = struct
  let scheme = Str.regexp "\\(.+\\)://\\(.+\\)"

  let domain =
    Str.regexp "\\([^/:\\?]*\\)\\(:[0-9]+\\)?\\(/[^?]*\\)?\\(\\?\\(.+\\)\\)?"

  let port = Str.regexp ".*:\\([0-9]+\\)"
  let path = Str.regexp ".*/\\(.*\\)"
  let query = Str.regexp ".*\\?\\(.+\\)"
end

let replace_path u u' = { u with path = u'.path }

let parse url host =
  let scheme =
    if Str.string_match Re.scheme url 0 then
      Str.replace_first Re.scheme "\\1" url
    else "gemini"
  and right_part =
    if Str.string_match Re.scheme url 0 then
      Str.replace_first Re.scheme "\\2" url
    else url
  in
  let domain = Str.replace_first Re.domain "\\1" right_part
  and port =
    if Str.string_match Re.port right_part 0 then
      Str.replace_first Re.domain "\\2" right_part
      |> Str.replace_first Re.port "\\1"
      |> int_of_string
    else 1965
  and path =
    if Str.string_match Re.path right_part 0 then
      Str.replace_first Re.domain "\\3" right_part
    else "/"
  and query =
    if Str.string_match Re.query right_part 0 then
      Str.replace_first Re.domain "\\5" right_part |> encode
    else ""
  in
  {
    scheme;
    domain =
      (if (host <> "" && domain <> host) || domain = "" then host else domain);
    port;
    path =
      (if host <> "" && domain <> host && path = "/" then "/" ^ domain
      else path);
    query;
  }

let to_string { scheme; domain; path; query; _ } =
  if query = "" then Printf.sprintf "%s://%s%s" scheme domain path
  else Printf.sprintf "%s://%s%s?%s" scheme domain path query