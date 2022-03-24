module type S = sig
  val usage_msg : Arg.usage_msg
  val address : string list ref
  val raw : bool ref
  val anon_fun : Arg.anon_fun
  val speclist : (Arg.key * Arg.spec * Arg.doc) list
  val parse_args : (string, Common.Err.t) result
end

module Default : S = struct
  let usage_msg = Printf.sprintf "%s [address] [flags]" Sys.argv.(0)
  let address = ref [ "" ]
  let raw = ref false
  let anon_fun new_address = address := new_address :: !address
  let speclist = [ ("--raw", Arg.Set raw, "Disable formatting") ]

  let parse_args =
    Arg.parse speclist anon_fun usage_msg;
    if List.length !address <= 2 then Ok (List.hd !address)
    else Error `TooManyAddressSpecified
end
