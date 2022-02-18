module type S = sig
  val input : string -> string
  val sensitive : string -> string
end

module Default : S = struct
  let input meta =
    Printf.printf "%s: " meta;
    flush stdout;
    Common.Urllib.encode @@ input_line stdin

  let sensitive meta = input meta
end
