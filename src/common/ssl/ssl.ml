type context

external init : unit -> unit = "init"
external create_context : unit -> context = "create_context"

external use_certificate : context -> string -> string -> unit
  = "use_certificate"

external use_certificate_from_string : context -> string -> string -> unit
  = "use_certificate_from_string"

external open_connection_with_context : context -> string -> int -> unit
  = "open_connection_with_context"

external output_string : context -> string -> unit = "output_string"
external input_char : context -> char = "input_char"
external get_error_string : unit -> string = "get_error_string"

type ssl_error =
  | Error_none
  | Error_ssl
  | Error_want_read
  | Error_want_write
  | Error_want_x509_lookup
  | Error_syscall
  | Error_zero_return
  | Error_want_connect
  | Error_want_accept

exception Context_error of string
exception Certificate_error of string
exception Private_key_error of string
exception Unmatching_keys
exception Connection_error of ssl_error
exception Read_error of ssl_error
exception Write_error of ssl_error
exception Socket_connection of string

let () =
  Printexc.register_printer (function
    | Context_error s -> Some ("SSL: Context error" ^ s)
    | Certificate_error s -> Some ("SSL: Certificate error: " ^ s)
    | Private_key_error s -> Some ("SSL: Private key error: " ^ s)
    | Unmatching_keys -> Some "SSL: Unmatching keys"
    | Connection_error _ ->
        Some ("SSL connection() error: " ^ get_error_string ())
    | Read_error _ -> Some ("SSL read() error: " ^ get_error_string ())
    | Write_error _ -> Some ("SSL write() error: " ^ get_error_string ())
    | Socket_connection s -> Some ("SSL: socket connection error: " ^ s)
    | _ -> None)

let () =
  Callback.register_exception "ssl_exn_context_error" (Context_error "");
  Callback.register_exception "ssl_exn_certificate_error" (Certificate_error "");
  Callback.register_exception "ssl_exn_private_key_error" (Private_key_error "");
  Callback.register_exception "ssl_exn_unmatching_keys" Unmatching_keys;
  Callback.register_exception "ssl_exn_connection_error"
    (Connection_error Error_none);
  Callback.register_exception "ssl_exn_read_error" (Read_error Error_none);
  Callback.register_exception "ssl_exn_write_error" (Write_error Error_none);
  Callback.register_exception "ssl_exn_socket_connection_error"
    (Socket_connection "")
