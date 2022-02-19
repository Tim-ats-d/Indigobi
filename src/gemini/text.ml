type t = line list

and line =
  | Text of string
  | Link of { url : string; name : string option }
  | Preformat of preformat
  | Heading of [ `H1 | `H2 | `H3 ]
  | ListItem of string
  | Quote of string

and preformat = { alt : string option; text : string }

let parse str =
  let rec loop acc is_preformat pf = function
    | [] -> List.rev acc
    | x :: xs ->
        let module S = String in
        if S.starts_with ~prefix:"```" x then
          if is_preformat then
            loop (Preformat pf :: acc) (not is_preformat)
              { alt = None; text = "" } xs
          else
            let alt_str = S.sub x 3 (S.length x - 3) in
            let alt = if alt_str = "" then None else Some alt_str in
            loop acc (not is_preformat) { pf with alt } xs
        else if is_preformat then
          loop acc is_preformat { pf with text = pf.text ^ x } xs
        else
          let x' =
            if S.get x 0 = '#' then Heading `H1
            else if S.starts_with ~prefix:"##" x then Heading `H2
            else if S.starts_with ~prefix:"###" x then Heading `H3
            else if S.starts_with ~prefix:"* " x then ListItem x
            else if S.get x 0 = '>' then Quote x
            else
              let post = S.sub x 2 (S.length x - 2) in
              if S.starts_with ~prefix:"=>" x && post <> "" then
                match Str.(split (regexp "[ \t]") @@ S.trim post) with
                | [ url ] -> Link { url; name = None }
                | [ url; name ] -> Link { url; name = Some name }
                | _ -> assert false
              else Text x
          in
          loop (x' :: acc) is_preformat pf xs
  in
  let lines = Str.(split (regexp "\n\\|\r\n") str) in
  loop [] false { alt = None; text = "" } lines
