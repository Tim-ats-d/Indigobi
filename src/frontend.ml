open Import

module type S = sig
  val get : url:string -> host:string -> unit
end

module Make (Cli : Cli.S) (Backend : Backend.S) = struct
  let get ~url ~host =
    let res = Backend.get ~url ~host in
    match res with
    | Ok (mime, body) -> Printf.printf "%s\n%s" mime body
    | Error (#G.Status.err as e) -> Cli.print_err @@ `GeminiErr e
    | Error (#Common.Err.t as e) -> Cli.print_err @@ `CommonErr e
end
