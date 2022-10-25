type t = line list

and line =
  | Text of string
  | Link of { url : string; name : string option }
  | Preformat of preformat
  | Heading of [ `H1 | `H2 | `H3 ] * string
  | Items of string
  | Quote of string

and preformat = { alt : string option; text : string }

module Regex = struct
  let spaces = Re.(rep (alt [ char ' '; char '\t' ]))
  let line re = Re.compile Re.(seq [ re; spaces; group (rep1 any) ])
  let h1 = line (Re.char '#')
  let h2 = line (Re.str "##")
  let h3 = line (Re.str "###")
  let quote = line (Re.char '>')
  let item = line (Re.str "* ")

  let link =
    Re.compile
      Re.(
        seq
          [
            str "=>";
            spaces;
            group (rep1 (diff print space));
            opt (seq [ spaces; group (rep1 any) ]);
          ])
end

let parse text =
  let rec loop acc is_preformat pf = function
    | [] -> List.rev acc
    | x :: xs -> (
        match (String.starts_with ~prefix:"```" x, is_preformat) with
        | true, true ->
            loop (Preformat pf :: acc) (not is_preformat)
              { alt = None; text = "" } xs
        | true, false ->
            let alt_str = String.sub x 3 (String.length x - 3) in
            let alt = if alt_str = "" then None else Some alt_str in
            loop acc (not is_preformat) { pf with alt } xs
        | false, true ->
            loop acc is_preformat { pf with text = pf.text ^ x ^ "\n" } xs
        | false, false ->
            let frgmt =
              if x = "" then Text ""
              else
                match Re.exec_opt Regex.h1 x with
                | Some grp -> Heading (`H1, Re.Group.get grp 1)
                | None -> (
                    match Re.exec_opt Regex.h2 x with
                    | Some grp -> Heading (`H2, Re.Group.get grp 1)
                    | None -> (
                        match Re.exec_opt Regex.h3 x with
                        | Some grp -> Heading (`H3, Re.Group.get grp 1)
                        | None -> (
                            match Re.exec_opt Regex.item x with
                            | Some grp -> Items (Re.Group.get grp 1)
                            | None -> (
                                match Re.exec_opt Regex.quote x with
                                | Some grp -> Quote (Re.Group.get grp 1)
                                | None -> (
                                    match Re.matches Regex.link x with
                                    | [] -> Text x
                                    | [ url ] -> Link { url; name = None }
                                    | [ url; name ] ->
                                        Link { url; name = Some name }
                                    | _ -> assert false)))))
            in
            loop (frgmt :: acc) is_preformat pf xs)
  in
  Re.(split (compile (alt [ char '\n'; str "\r\n" ]))) text
  |> loop [] false { alt = None; text = "" }
