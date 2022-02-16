type t =
  | Input
  | SensitiveInput
  | Sucess
  | RedirectTemporary
  | RedirectPermanent
  | TemporaryFailure
  | ServerUnavailable
  | CGIError
  | ProxyError
  | SlowDown of int
  | PermanentFailure
  | NotFound
  | Gone
  | ProxyRequestRefused
  | BadRequest
  | ClientCertificateRequired
  | CertificateNotAuthorised
  | CertificateNotValid
