type t = { status : Status.t; meta : string }

let parse str =
  let re = Str.regexp "\\([0-9][0-9]\\) \\(.*\\)\r\n" in
  if Str.string_match re str 0 then
    let meta = Str.matched_group 2 str in
    if Bytes.(length @@ of_string meta) > 1024 then None
    else
      let status =
        Str.matched_group 1 str |> int_of_string |> Status.of_int meta
      in
      Some { status; meta }
  else None
