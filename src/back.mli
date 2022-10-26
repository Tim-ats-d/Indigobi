open Common
open Backend

module type S = sig
  val get :
    ?bypass:Request.bypass ->
    url:string ->
    host:string ->
    port:int ->
    cert:Tls.Config.own_cert option ->
    float ->
    (Mime.t * string, [> Err.back | Status.err ]) Lwt_result.t

  val cert_from_file :
    string -> (Tls.Config.own_cert option, [> Common.Err.back ]) Lwt_result.t
end

module Make : functor
  (Prompt : Frontend.Prompt.S)
  (Requester : Backend.Requester.S)
  -> S
