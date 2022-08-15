module type S = sig
  val get :
    url:string ->
    host:string ->
    port:int ->
    cert:Tls.Config.own_cert option ->
    timeout:float ->
    ( Back.Mime.t * string,
      [> Common.Err.back | Gemini.Status.err ] )
    Lwt_result.t

  val cert_from_file :
    string -> (Tls.Config.own_cert option, [> Common.Err.back ]) Lwt_result.t
end

module Make : functor
  (Prompt : Front.Prompt.S)
  (Requester : Back.Requester.S)
  -> S
