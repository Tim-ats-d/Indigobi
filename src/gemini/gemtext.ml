type t = line list

and line =
  | Text of string
  | Link of { url : string; name : string option }
  | Preformat of preformat
  | Header of header * string
  | ListItem of string
  | Quote of string

and header = H1 | H2 | H3
and preformat = { alt : string option; text : string }

type headers = (header * string) list

let h1 = Str.regexp {|#[ \t]*\([^#].+\)|}
and h2 = Str.regexp {|##[ \t]*\([^#].+\)|}
and h3 = Str.regexp {|###[ \t]*\([^#].+\)|}
and quote = Str.regexp {|>[ \t]*\(.+\)|}
and item = Str.regexp {|* \(.*\)|}

let parse str =
  let module S = String in
  let i = ref (-1) in
  let rec loop acc is_preformat pf bmarks =
    incr i;
    function
    | [] -> (List.rev acc, bmarks)
    | x :: xs ->
        if S.starts_with ~prefix:"```" x then
          if is_preformat then
            loop (Preformat pf :: acc) (not is_preformat)
              { alt = None; text = "" } bmarks xs
          else
            let alt_str = S.sub x 3 (S.length x - 3) in
            let alt = if alt_str = "" then None else Some alt_str in
            loop acc (not is_preformat) { pf with alt } bmarks xs
        else if is_preformat then
          loop acc is_preformat { pf with text = pf.text ^ x ^ "\n" } bmarks xs
        else
          let x, meta =
            if x = "" then (Text "", bmarks)
            else if Str.string_match h1 x 0 then
              let grp = Str.matched_group 1 x in
              (Header (H1, grp), (H1, grp) :: bmarks)
            else if Str.string_match h2 x 0 then
              let grp = Str.matched_group 1 x in
              (Header (H2, grp), (H2, grp) :: bmarks)
            else if Str.string_match h3 x 0 then
              let grp = Str.matched_group 1 x in
              (Header (H3, grp), (H3, grp) :: bmarks)
            else if Str.string_match item x 0 then
              (ListItem (Str.matched_group 1 x), bmarks)
            else if Str.string_match quote x 0 then
              (Quote (Str.matched_group 1 x), bmarks)
            else
              let post = S.trim @@ S.sub x 2 (S.length x - 2) in
              if S.starts_with ~prefix:"=>" x && post <> "" then
                match Str.(bounded_split (regexp "[ \t]+") post 2) with
                | [ url ] -> (Link { url; name = None }, bmarks)
                | [ url; name ] -> (Link { url; name = Some name }, bmarks)
                | _ -> assert false
              else (Text x, bmarks)
          in
          loop (x :: acc) is_preformat pf meta xs
  in
  let lines = Str.(split (regexp "\n\\|\r\n") str) in
  loop [] false { alt = None; text = "" } [] lines
