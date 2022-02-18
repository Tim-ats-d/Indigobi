module type S = sig
  val input : unit -> string
  val sensitive : unit -> string
end

module Default : S = struct
  let input () =
    print_string "INPUT: ";
    flush stdout;
    input_line stdin

  let sensitive = input
end
