type t = {
  mutable address : string option;
  mutable raw : bool;
  mutable certificate : string;
}

module type S = sig
  val parse : unit -> t
end

module Default : S = struct
  let usage_msg = Printf.sprintf "%s [OPTION]... [ADRESS]" Sys.argv.(0)
  let default = { address = None; raw = false; certificate = "" }

  let speclist =
    [
      ("--raw", Arg.Unit (fun () -> default.raw <- true), "Disable formatting");
      ( "--cert",
        Arg.String (fun certificate -> default.certificate <- certificate),
        "Attach client certificate" );
    ]

  let parse () =
    Arg.parse speclist (fun adrr -> default.address <- Some adrr) usage_msg;
    default
end
