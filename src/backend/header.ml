type t = { status : Status.t; meta : string }

let re =
  Re.compile
    Re.(seq [ group (seq [ digit; digit ]); char ' '; group (rep1 any) ])

let parse head =
  match Re.exec_opt re head with
  | None -> Error `MalformedHeader
  | Some grp -> (
      let meta = Re.Group.get grp 2 in
      if meta |> Bytes.of_string |> Bytes.length > 1024 then
        Error `TooLongHeader
      else
        match Re.Group.get grp 1 |> int_of_string |> Status.from_int head with
        | Ok status -> Ok { status; meta }
        | Error #Common.Err.status_code as code -> code)
