module type S = sig
  type t = {
    mutable address : string list;
    mutable raw : bool;
  }
  
  val params : t
  val parse_args : (string, Common.Err.t) result
end

module Default : S = struct
  type t = {
    mutable address : string list;
    mutable raw : bool;
  }

  let usage_msg = Printf.sprintf "%s [address] [flags]" Sys.argv.(0)
  
  let params = {
    address = [""];
    raw = false;
  }

  let anon_fun new_address = params.address <- new_address :: params.address
  let speclist = [ ("--raw", Arg.Bool (fun b -> params.raw <- b), "Disable formatting") ]

  let parse_args =
    Arg.parse speclist anon_fun usage_msg;
    if List.length params.address <= 2 then Ok (List.hd params.address)
    else Error `TooManyAddressSpecified
end
