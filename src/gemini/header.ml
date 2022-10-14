type t = { status : Status.t; meta : string }

let re = Str.regexp "\\([0-9][0-9]\\) \\(.*\\)"

let parse str =
  if Str.string_match re str 0 then
    let meta = Str.matched_group 2 str in
    if Bytes.(length @@ of_string meta) > 1024 then Error `TooLongHeader
    else
      match
        Str.matched_group 1 str |> int_of_string |> Status.from_int meta
      with
      | Ok status -> Ok { status; meta }
      | Error #Common.Err.status_code as code -> code
  else Error `MalformedHeader
