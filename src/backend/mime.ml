type t = Gemini | Text of string | Other of string

let mime_re = Re.compile Re.(seq [ str "text/"; group (rep any) ])

let guess mime =
  if mime = "text/gemini" then Gemini
  else
    match Re.exec_opt mime_re mime with
    | None -> Other mime
    | Some grp -> Text (Re.Group.get grp 1)

let parse str =
  if str = "" then Gemini
  else
    match String.split_on_char ';' str with
    | [ mtype ] -> guess mtype
    | _ -> Gemini
