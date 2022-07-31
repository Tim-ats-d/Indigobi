type t = line list

and line =
  | Text of string
  | Link of { url : string; name : string option }
  | Preformat of preformat
  | Heading of [ `H1 | `H2 | `H3 ] * string
  | ListItem of string
  | Quote of string

and preformat = { alt : string option; text : string }

module Re = struct
  let h1 = Str.regexp {|#[ \t]*\([^#].+\)|}
  let h2 = Str.regexp {|##[ \t]*\([^#].+\)|}
  let h3 = Str.regexp {|###[ \t]*\([^#].+\)|}
  let quote = Str.regexp {|>[ \t]*\(.+\)|}
  let item = Str.regexp {|"* \(.*\)|}
end

let parse str =
  let module S = String in
  let rec loop acc is_preformat pf = function
    | [] -> List.rev acc
    | x :: xs ->
        if S.starts_with ~prefix:"```" x then
          if is_preformat then
            loop (Preformat pf :: acc) (not is_preformat)
              { alt = None; text = "" } xs
          else
            let alt_str = S.sub x 3 (S.length x - 3) in
            let alt = if alt_str = "" then None else Some alt_str in
            loop acc (not is_preformat) { pf with alt } xs
        else if is_preformat then
          loop acc is_preformat { pf with text = pf.text ^ x ^ "\n" } xs
        else
          let x' =
            if x = "" then Text ""
            else if Str.string_match Re.h1 x 0 then
              Heading (`H1, Str.matched_group 1 x)
            else if Str.string_match Re.h2 x 0 then
              Heading (`H2, Str.matched_group 1 x)
            else if Str.string_match Re.h3 x 0 then
              Heading (`H3, Str.matched_group 1 x)
            else if Str.string_match Re.item x 0 then
              ListItem (Str.matched_group 1 x)
            else if Str.string_match Re.quote x 0 then
              Quote (Str.matched_group 1 x)
            else
              let post = S.trim @@ S.sub x 2 (S.length x - 2) in
              if S.starts_with ~prefix:"=>" x && post <> "" then
                match Str.(bounded_split (regexp "[ \t]+") post 2) with
                | [ url ] -> Link { url; name = None }
                | [ url; name ] -> Link { url; name = Some name }
                | _ -> assert false
              else Text x
          in
          loop (x' :: acc) is_preformat pf xs
  in
  let lines = Str.(split (regexp "\n\\|\r\n") str) in
  loop [] false { alt = None; text = "" } lines
