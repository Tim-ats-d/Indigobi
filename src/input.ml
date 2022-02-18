module type S = sig
  val input : string -> string
  val sensitive : string -> string
end

module Default : S = struct
  let input meta =
    Printf.printf "%s: " meta;
    flush stdout;
    input_line stdin |> Urllib.encode

  let sensitive meta = input meta
end
