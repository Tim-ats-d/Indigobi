type t = {
  scheme : string;
  domain : string;
  port : int;
  path : string;
  query : string;
}

let encode input_str =
  let convert_char str chr =
    let new_chr =
      match chr with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> Printf.sprintf "%c" chr
      | _ -> Printf.sprintf "%%%02X" @@ Char.code chr
    in
    str ^ new_chr
  in
  String.fold_left convert_char "" input_str

let parse url host =
  let scheme_re = Str.regexp "\\(.+\\)://\\(.+\\)"
  and domain_re =
    Str.regexp "\\([^/:\\?]*\\)\\(:[0-9]+\\)?\\(/[^?]*\\)?\\(\\?\\(.+\\)\\)?"
  and port_re = Str.regexp ".*:\\([0-9]+\\)"
  and path_re = Str.regexp ".*/\\(.*\\)"
  and query_re = Str.regexp ".*\\?\\(.+\\)" in
  let scheme =
    if Str.string_match scheme_re url 0 then
      Str.replace_first scheme_re "\\1" url
    else "gemini"
  and right_part =
    if Str.string_match scheme_re url 0 then
      Str.replace_first scheme_re "\\2" url
    else url
  in
  let domain = Str.replace_first domain_re "\\1" right_part
  and port =
    if Str.string_match port_re right_part 0 then
      Str.replace_first domain_re "\\2" right_part
      |> Str.replace_first port_re "\\1"
      |> int_of_string
    else -1
  and path =
    if Str.string_match path_re right_part 0 then
      Str.replace_first domain_re "\\3" right_part
    else "/"
  and query =
    if Str.string_match query_re right_part 0 then
      Str.replace_first domain_re "\\5" right_part |> encode
    else ""
  in
  {
    scheme;
    domain =
      (if (host <> "" && domain <> host) || domain = "" then host else domain);
    port = (if port = -1 then 1965 else port);
    path =
      (if host <> "" && domain <> host && path = "/" then "/" ^ domain
      else path);
    query;
  }

let to_string { scheme; domain; path; query; _ } =
  if query = "" then Printf.sprintf "%s://%s%s" scheme domain path
  else Printf.sprintf "%s://%s%s?%s" scheme domain path query
