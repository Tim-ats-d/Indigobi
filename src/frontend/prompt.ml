module type S = sig
  val prompt : string -> string Lwt.t
  val prompt_sensitive : string -> string Lwt.t
  val prompt_bool : string -> bool Lwt.t
end

module Make (Printer : Printer.S) = struct
  open Lwt.Syntax

  let prompt meta =
    let* () = Lwt_io.print meta in
    Lwt_io.(read stdin)

  let prompt_sensitive _meta = failwith "todo"
  let prompt_bool _meta = failwith "todo"
end
