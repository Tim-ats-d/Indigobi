module type S = sig
  val input : string -> string
  val sensitive : string -> string
  val normalize : string -> string
end

module Default : S = struct
  let rec input meta =
    Printf.printf "%s: " meta;
    flush stdout;
    input_line stdin |> normalize

  and normalize input_str =
    let convert_char str chr =
      let new_chr =
        match chr with
        | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> Printf.sprintf "%c" chr
        | _ -> Printf.sprintf "%%%02X" @@ Char.code chr
      in
      str ^ new_chr
    in
    String.fold_left convert_char "" input_str

  let sensitive meta = input meta
end
