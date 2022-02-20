open Import

module type S = sig
  val get : url:string -> host:string -> unit
end

module Make (Cli : Cli.S) (Backend : Backend.S) = struct
  let get ~url ~host =
    match Backend.get ~url ~host with
    | Ok (_mime, body) -> Cli.print_gemini @@ Gemini.Text.parse body
    | Error (#G.Status.err as e) -> Cli.print_err @@ `GeminiErr e
    | Error (#Common.Err.t as e) -> Cli.print_err @@ `CommonErr e
end
